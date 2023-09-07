#' Transition between different filters
#'
#' This transition allows you to transition between a range of filtering
#' conditions. The conditions are expressed as logical statements and rows in
#' the data will be retained if the statement evaluates to `TRUE`. It is
#' possible to keep filtered data on display by setting `keep = TRUE` which will
#' let data be retained as the result of the exit function. Note that if data is
#' kept the enter function will have no effect.
#'
#' @param transition_length The relative length of the transition. Will be
#' recycled to match the number of states in the data
#' @param filter_length The relative length of the pause at the states. Will be
#' recycled to match the number of states in the data
#' @param ... A number of expressions to be evaluated in the context of the layer
#' data, returning a logical vector. If the expressions are named, the name will
#' be available as a frame variable.
#' @param wrap Should the animation *wrap-around*? If `TRUE` the last filter will
#' be transitioned into the first.
#' @param keep Should rows that evaluates to `FALSE` be kept in the data as it
#' looks after exit has been applied
#'
#' @section Label variables:
#' `transition_filter` makes the following variables available for string
#' literal interpretation, in addition to the general ones provided by
#' [animate()]:
#'
#' - **transitioning** is a boolean indicating whether the frame is part of the
#'   transitioning phase
#' - **previous_filter** The name of the last filter the animation was at
#' - **closest_filter** The name of the filter closest to this frame
#' - **next_filter** The name of the next filter the animation will be part of
#' - **previous_expression** The expression of the last filter the animation was at
#' - **closest_expression** The expression of the filter closest to this frame
#' - **next_expression** The expression of the next filter the animation will be part of
#'
#' @section Object permanence:
#' `transition_filter` does not link rows across data to the same graphic
#' element, so elements will be defined uniquely by each row. If `keep = TRUE`
#' the rows not matching the conditions of a filter is not removed from the plot
#' after the exit animation, and a possible subsequent enter will begin from
#' the state they were left in, rather than enter anew from the state defined by
#' the enter function.
#'
#' @inheritSection transition_states Computed Variables
#'
#' @family transitions
#'
#' @importFrom rlang quos quos_auto_name
#' @importFrom ggplot2 ggproto
#' @export
#'
#' @examples
#' anim <- ggplot(iris, aes(Petal.Width, Petal.Length, colour = Species)) +
#'   geom_point() +
#'   transition_filter(
#'     transition_length = 2,
#'     filter_length = 1,
#'     Setosa = Species == 'setosa',
#'     Long = Petal.Length > 4,
#'     Wide = Petal.Width > 2
#'   ) +
#'   ggtitle(
#'     'Filter: {closest_filter}',
#'     subtitle = '{closest_expression}'
#'   ) +
#'   enter_fade() +
#'   exit_fly(y_loc = 0)
#'
#' # Setting `keep = TRUE` allows you to keep the culled data on display. Only
#' # exit functions will be used in that case (as elements enters from the
#' # result of the exit function)
#' anim2 <- ggplot(iris, aes(Petal.Width, Petal.Length, colour = Species)) +
#'   geom_point() +
#'   transition_filter(
#'     transition_length = 2,
#'     filter_length = 1,
#'     Setosa = Species == 'setosa',
#'     Long = Petal.Length > 4,
#'     Wide = Petal.Width > 2,
#'     keep = TRUE
#'   ) +
#'   ggtitle(
#'     'Filter: {closest_filter}',
#'     subtitle = '{closest_expression}'
#'   ) +
#'   exit_recolour(colour = 'grey') +
#'   exit_shrink(size = 0.5)
#'
transition_filter <- function(transition_length = 1, filter_length = 1, ..., wrap = TRUE, keep = FALSE) {
  filter_quos <- quos_auto_name(quos(...))
  if (length(filter_quos) < 2) {
    cli::cli_abort('{.fun transition_filter} requires at least 2 filtering conditions')
  }
  ggproto(NULL, TransitionFilter,
    params = list(
      filter_quos = filter_quos,
      transition_length = transition_length,
      filter_length = filter_length,
      wrap = wrap,
      keep = keep
    )
  )
}
#' @rdname gganimate-ggproto
#' @format NULL
#' @usage NULL
#' @export
#' @importFrom ggplot2 ggproto
#' @importFrom stringi stri_match
#' @importFrom tweenr tween_state keep_state
#' @importFrom rlang quo_name
TransitionFilter <- ggproto('TransitionFilter', Transition,
  mapping = '(.+)',
  var_names = 'filter',
  setup_params = function(self, data, params) {
    params$row_id <- assign_filters(data, params$filter_quos)
    params$require_stat <- any(vapply(params$filter_quo, function(f) require_stat(rlang::quo_get_expr(f)), logical(1)))
    params
  },
  setup_params2 = function(self, data, params, row_vars) {
    params$row_id <- assign_filters(data, params$filter_quos, TRUE, row_vars)
    transition_length <- rep(params$transition_length, length.out = length(params$filter_quos))
    if (!params$wrap) transition_length[length(transition_length)] <- 0
    filter_length <- rep(params$filter_length, length.out = length(params$filter_quos))
    frames <- distribute_frames(filter_length, transition_length, params$nframes + if (params$wrap) 1 else 0)
    params$state_length <- frames$static_length
    params$transition_length <- frames$transition_length
    params$frame_info <- cbind(
      get_frame_info(
        static_levels = names(params$filter_quos),
        static_lengths = params$state_length,
        transition_lengths = params$transition_length,
        nframes = params$nframes,
        static_first = TRUE,
        static_name = 'filter'
      ),
      get_frame_info(
        static_levels = vapply(params$filter_quos, quo_name, character(1)),
        static_lengths = params$state_length,
        transition_lengths = params$transition_length,
        nframes = params$nframes,
        static_first = TRUE,
        static_name = 'expression'
      )
    )
    params$nframes <- nrow(params$frame_info)
    params
  },
  expand_panel = function(self, data, type, id, match, ease, enter, exit, params, layer_index) {
    row_vars <- self$get_row_vars(data)
    if (is.null(row_vars)) return(data)
    data$group <- paste0(row_vars$before, row_vars$after)
    if (type %in% c('point', 'sf')) {
      data$.temp_id <- seq_len(nrow(data))
      id <- quo(.temp_id)
      ease <- c(ease, 'linear')
    }
    filter <- strsplit(row_vars$filter, '-')
    row <- rep(seq_along(filter), lengths(filter))
    filter <- as.integer(unlist(filter))
    present <- filter != 0 & !is.na(filter)
    row <- row[present]
    filter <- filter[present]

    filtered_data <- lapply(seq_along(params$filter_quos), function(i) {
      include <- row[filter == i]
      exclude <- setdiff(seq_len(nrow(data)), include)
      d_f <- data
      if (params$keep) {
        exit_data <- exit(d_f[exclude, , drop = FALSE])
        if (is.null(exit_data)) {
          d_f <- d_f[include, , drop = FALSE]
        } else {
          d_f[exclude, ] <- exit_data
        }
      } else {
        d_f <- d_f[include, , drop = FALSE]
      }
      d_f
    })
    all_frames <- filtered_data[[1]]
    for (i in seq_along(filtered_data)) {
      if (params$state_length[i] != 0) {
        all_frames <- keep_state(all_frames, params$state_length[i])
      }
      if (params$transition_length[i] != 0) {
        next_filter <- if (i == length(filtered_data)) filtered_data[[1]] else filtered_data[[i + 1]]
        all_frames <- switch(
          type,
          point = tween_state(all_frames, next_filter, ease, params$transition_length[i], !!id, enter, exit),
          path = transform_path(all_frames, next_filter, ease, params$transition_length[i], !!id, enter, exit, match),
          polygon = transform_polygon(all_frames, next_filter, ease, params$transition_length[i], !!id, enter, exit, match),
          sf = transform_sf(all_frames, next_filter, ease, params$transition_length[i], !!id, enter, exit),
          cli::cli_abort('{type} layers not currently supported by {.fun transition_filter}')
        )
      }
    }
    if (params$wrap) {
      all_frames <- all_frames[all_frames$.frame <= params$nframes, ]
    }
    all_frames$group <- paste0(all_frames$group, '<', all_frames$.frame, '>')
    all_frames$.frame <- NULL
    all_frames
  }
)

assign_filters <- function(data, filters, after = FALSE, row_vars = NULL) {
  do_filter <- vapply(filters, function(f) require_stat(rlang::quo_get_expr(f)), logical(1)) == after
  row_filters <- lapply(data, function(d) {
    row_filter <- vec_rbind0(!!!lapply(seq_along(filters), function(i) {
      if (!do_filter[i]) return(rep(FALSE, nrow(d)))
      filter <- safe_eval(filters[[i]], d)
      filter <- filter %||% rep(TRUE, nrow(d))
      if (!is_logical(filter)) cli::cli_abort('Filters must return a logical vector')
      filter
    }))
    if (all(row_filter)) return(numeric(0))
    apply(row_filter, 2, function(x) if (!any(x, na.rm = TRUE)) '0' else paste(which(x), collapse = '-'))
  })
  if (after) {
    Map(function(new_f, old_f) paste0(old_f, '-', new_f), new_f = row_filters, old_f = row_vars$filter)
  } else {
    row_filters
  }
}
