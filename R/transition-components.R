#' Transition individual components through their own lifecycle
#'
#' This transition allows individual visual components to define their own
#' "life-cycle". This means that the final animation will not have any common
#' "state" and "transition" phase as any component can be moving or static at
#' any point in time.
#'
#' @param time The unquoted name of the column holding the time for each state
#' of the components
#' @param range The range the animation should span. Defaults to the range of
#' time plus enter and exit length
#' @param enter_length,exit_length How long time should be spend on enter and
#' exit transitions. Defaults to 0
#'
#' @section Label variables:
#' `transition_components` makes the following variables available for string
#' literal interpretation, in addition to the general ones provided by
#' [animate()]:
#'
#' - **frame_time** gives the time that the current frame corresponds to
#'
#' @section Object permanence:
#' `transition_components` uses the group aesthetic of each layer to identify
#' which rows in the input data correspond to the same graphic element and will
#' therefore define stages in time that the element will animate through. The
#' group aesthetic, if not set, will be calculated from the interaction of all
#' discrete aesthetics in the layer (excluding `label`), so it is often better
#' to set it explicitly when animating, to make sure your data is interpreted in
#' the right way. If the group aesthetic is not set, and no discrete aesthetics
#' exists then all rows will have the same group.
#'
#' @inheritSection transition_states Computed Variables
#'
#' @family transitions
#'
#' @importFrom rlang enquo
#' @importFrom ggplot2 ggproto
#' @export
#'
#' @examples
#' data <- data.frame(
#'   x = runif(10),
#'   y = runif(10),
#'   size = sample(1:3, 10, TRUE),
#'   time = c(1, 4, 6, 7, 9, 6, 7, 8, 9, 10),
#'   id = rep(1:2, each = 5)
#' )
#'
#' anim <- ggplot(data, aes(x, y, group = id, size = size)) +
#'   geom_point() +
#'   transition_components(time)
#'
#' # By default the time range is set to the range of the time variable (plus
#' # any enter and exit length), but this can be overwritten
#' anim2 <- ggplot(data, aes(x, y, group = id, size = size)) +
#'   geom_point() +
#'   transition_components(time, range = c(4, 8))
#'
#' # If you are using any enter/exit functions you need to give them some time
#' anim3 <- ggplot(data, aes(x, y, group = id, size = size)) +
#'   geom_point() +
#'   transition_components(time, enter_length = 2, exit_length = 2) +
#'   enter_grow() +
#'   exit_fade()
#'
transition_components <- function(time, range = NULL, enter_length = NULL, exit_length = NULL) {
  time_quo <- enquo(time)
  require_quo(time_quo, 'time')
  ggproto(NULL, TransitionComponents,
          params = list(
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
TransitionComponents <- ggproto('TransitionComponents', Transition,
  mapping = '(.+?)',
  var_names = 'time',
  setup_params = function(self, data, params) {
    params$time <- get_row_comp_time(data, params$time_quo, params)
    params$require_stat <- is_placeholder(params$time)
    static <- lengths(params$time$values) == 0
    params$row_id <- Map(function(t, s) if (s) character() else t,
                         t = params$time$values, s = static)
    params
  },
  setup_params2 = function(self, data, params, row_vars) {
    if (is_placeholder(params$time)) {
      params$time <- get_row_comp_time(data, params$time_quo, params, after = TRUE)
    } else {
      params$time$values <- row_vars$time
    }
    static <- lengths(params$time$values) == 0

    params$enter_length <- params$time$enter_length
    params$exit_length <- params$time$exit_length
    params$range <- params$time$range
    params$frame_time <- params$time$frame_time
    params$row_id <- Map(function(t, s) if (s) character() else t,
                         t = params$time$values, s = static)
    params$frame_info <- data_frame0(
      frame_time = params$time$frame_time
    )
    params$nframes <- nrow(params$frame_info)
    params
  },
  expand_panel = function(self, data, type, id, match, ease, enter, exit, params, layer_index) {
    row_vars <- self$get_row_vars(data)
    if (is.null(row_vars)) return(data)
    data$group <- paste0(row_vars$before, row_vars$after)
    time <- as.numeric(row_vars$time)
    all_frames <- switch(
      type,
      point = tween_components(data, ease, params$nframes, !!time, group, c(1, params$nframes), enter, exit, params$enter_length, params$exit_length),
      cli::cli_abort('{type} layers not currently supported by {.fun transition_components}')
    )
    all_frames$group <- paste0(all_frames$group, '<', all_frames$.frame, '>')
    all_frames$.frame <- NULL
    all_frames
  }
)
get_row_comp_time <- function(data, quo, params, after = FALSE) {
  if (!after && require_stat(rlang::quo_get_expr(quo))) {
    return(eval_placeholder(data))
  }
  row_time <- lapply(data, safe_eval, expr = quo)
  standard_times <- standardise_times(row_time, 'time')

  enter_length <- if (is.null(params$enter_length)) {
    0
  } else {
    if (!inherits(params$enter_length, standard_times$class)) {
      cli::cli_abort('{.arg enter_length} must be given in the same class as {.field time}')
    }
    as.numeric(params$enter_length)
  }
  exit_length <- if (is.null(params$exit_length)) {
    0
  } else {
    if (!inherits(params$exit_length, standard_times$class)) {
      cli::cli_abort('{.arg exit_length} must be given in the same class as {.field time}')
    }
    as.numeric(params$exit_length)
  }
  range <- if (is.null(params$range)) {
    range(unlist(standard_times$times)) + c(-enter_length, exit_length)
  } else {
    if (!inherits(params$range, standard_times$class)) {
      cli::cli_abort('{.arg range} must be given in the same class as {.field time}')
    }
    as.numeric(params$range)
  }
  full_length <- diff(range)
  frames <- lapply(standard_times$times, function(x) {
    if (length(x) == 0) return(numeric())
    frame <- round((params$nframes - 1) * (x - range[1])/full_length) + 1
    nc <- nchar(max(frame)) + 1
    sprintf(paste0('%0', nc, 'i'), frame)
  })
  frame_time <- recast_times(
    seq(range[1], range[2], length.out = params$nframes),
    standard_times$class
  )
  frame_length <- full_length / params$nframes
  list(
    values = frames,
    frame_time = frame_time,
    enter_length = round(enter_length / frame_length),
    exit_length = round(exit_length / frame_length)
  )
}
