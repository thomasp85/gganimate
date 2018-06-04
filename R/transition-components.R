#' @include transition-manual.R
NULL

#' Transition individual components through their own lifecycle
#'
#' This transition allows individual visual components to define their own
#' "life-cycle". This means that the final animation will not have any commen
#' "state" and "transition" phase as any component can be moving or static at
#' any point in time.
#'
#' @param id The unquoted name of the column holding the id that links
#' components across the data
#' @param time The unquoted name of the column holding the time for each state
#' of the components
#' @param range The range the animation should span. Defaults to the range of
#' time plus enter and exit length
#' @param enter_length,exit_length How long time should be spend on enter and
#' exit transitions. Defaults to 5\% of the range
#'
#' @section Label variables:
#' `transition_components` makes the following variables available for string
#' literal interpretation:
#'
#' - **frame_time** gives the time that the current frame corresponds to
#'
#' @family transitions
#'
#' @export
#' @importFrom rlang enquo
#' @importFrom ggplot2 ggproto
transition_components <- function(id, time, range = NULL, enter_length = NULL, exit_length = NULL) {
  id_quo <- enquo(id)
  time_quo <- enquo(time)
  ggproto(NULL, TransitionComponents,
          params = list(
            id_quo = id_quo,
            time_quo = time_quo,
            range = range,
            enter_length = enter_length,
            exit_length = exit_length
          )
  )
}
#' @rdname gganimate-ggproto
#' @format NULL
#' @usage NULL
#' @export
#' @importFrom ggplot2 ggproto
#' @importFrom stringi stri_match
#' @importFrom tweenr tween_components
TransitionComponents <- ggproto('TransitionComponents', TransitionManual,
  setup_params = function(self, data, params) {
    data_info <- component_info(data, params)

    params$enter_length <- data_info$enter_length
    params$exit_length <- data_info$exit_length
    params$range <- data_info$range
    params$frame_time <- data_info$frame_time
    params$row_id <- Map(function(t, i) paste0(t, '-', i), t = data_info$row_frame, i = data_info$row_id)
    params$frame_info <- data.frame(
      frame_time = data_info$frame_time
    )
    params$nframes <- nrow(params$frame_info)
    params
  },
  expand_data = function(self, data, type, ease, enter, exit, params, layer_index) {
    Map(function(d, t, en, ex, es) {
      split_panel <- stri_match(d$group, regex = '^(.+)_(.+?)-(.+)$')
      if (is.na(split_panel[1])) return(d)
      d$group <- as.integer(split_panel[, 2])
      time <- as.integer(split_panel[, 3])
      id <- as.integer(split_panel[, 4])
      all_frames <- switch(
        t,
        point = tween_components(d, es, params$nframes, !!time, !!id, params$range, en, ex, params$enter_length, params$exit_length),
        #path = tween_path(all_frames, next_state, es, params$transition_length[i], id, en, ex),
        #polygon = tween_polygon(all_frames, next_state, es, params$transition_length[i], id, en, ex),
        #sf = tween_sf(all_frames, next_state, es, params$transition_length[i], id, en, ex),
        stop("Unknown layer type", call. = FALSE)
      )
      all_frames$group <- paste0(all_frames$group, '_', all_frames$.frame)
      all_frames$.frame <- NULL
      all_frames
    }, d = data, t = type, en = enter, ex = exit, es = ease)
  }
)

#' @importFrom rlang eval_tidy
component_info <- function(data, params) {
  row_id <- lapply(data, eval_tidy, expr = params$id_quo)
  row_time <- lapply(data, eval_tidy, expr = params$time_quo)
  static_layer <- unlist(Map(function(data, id, time) {
    (length(id) != 1 && length(id) != nrow(data)) || (length(time) != 1 && length(time) != nrow(data))
  }, data = data, id = row_id, time = row_time))
  if (all(static_layer)) {
    stop('At least one layer must be in transition', call. = FALSE)
  }
  row_id[static_layer] <- list(character(0))
  row_time[static_layer] <- list(numeric(0))
  standard_times <- standardise_times(row_time)
  range <- if (is.null(params$range)) {
    range(unlist(standard_times$times))
  } else {
    if (!inherits(params$range, standard_times$class)) {
      stop('range must be given in the same class as time', call. = FALSE)
    }
    as.numeric(params$range)
  }
  full_length <- diff(range)
  enter_length <- if (is.null(params$enter_length)) {
    full_length * 0.05
  } else {
    if (!inherits(params$enter_length, standard_times$class)) {
      stop('enter_length must be given in the same class as time', call. = FALSE)
    }
    as.numeric(params$enter_length)
  }
  exit_length <- if (is.null(params$exit_length)) {
    full_length * 0.05
  } else {
    if (!inherits(params$exit_length, standard_times$class)) {
      stop('exit_length must be given in the same class as time', call. = FALSE)
    }
    as.numeric(params$exit_length)
  }
  if (is.null(params$range)) {
    range <- range + c(-enter_length, exit_length)
    full_length <- diff(range)
  }
  row_frame <- lapply(standard_times$times, function(x) {
    round((params$nframes - 1) * (x - range[1])/full_length) + 1
  })
  frame_time <- recast_times(
    seq(range[1], range[2], length.out = params$nframes),
    standard_times$class
  )
  frame_length <- params$nframes / full_length
  list(
    row_id = row_id,
    row_frame = row_frame,
    frame_time = frame_time,
    enter_length = enter_length * frame_length,
    exit_length = exit_length * frame_length
  )
}

standardise_times <- function(times) {
  possible_classes <- c('integer', 'numeric', 'POSIXct', 'Date')
  classes <- vapply(times[lengths(times) != 0], function(x) {
    cl <- inherits(x, possible_classes, which = TRUE)
    which(cl != 0 & cl == min(cl[cl != 0]))[1]
  }, integer(1))
  if (anyNA(classes)) stop('time column must either be ', paste0(possible_classes[-length(possible_classes)], collapse = ', '), ', or', possible_classes[length(possible_classes)], call. = FALSE)
  if (length(unique(classes)) != 1) stop('time column must be the same class in all layers', call. = FALSE)
  list(
    times = lapply(times, as.numeric),
    class = possible_classes[unique(classes)]
  )
}
recast_times <- function(time, class) {
  switch(
    class,
    integer = as.integer(round(time)),
    numeric =  time,
    POSIXct = structure(time, class = c('POSIXct', 'POSIXt')),
    Date = structure(time, class = 'Date')
  )
}
