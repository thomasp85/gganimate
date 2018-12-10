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
#' @param id **Deprecated**
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
transition_components <- function(time, range = NULL, enter_length = NULL, exit_length = NULL, id) {
  if (!missing(id)) warning('The `id` argument has been deprecated. Set `id` in each layer with the `group` aesthetic', call. = FALSE)
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
    params$frame_info <- data.frame(
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
      stop("Unsupported layer type", call. = FALSE)
    )
    all_frames$group <- paste0(all_frames$group, '<', all_frames$.frame, '>')
    all_frames$.frame <- NULL
    all_frames
  }
)
get_row_comp_time <- function(data, quo, params, after = FALSE) {
  if (!after && require_stat(quo[[2]])) {
    return(eval_placeholder(data))
  }
  row_time <- lapply(data, safe_eval, expr = quo)
  standard_times <- standardise_times(row_time, 'time')

  enter_length <- if (is.null(params$enter_length)) {
    0
  } else {
    if (!inherits(params$enter_length, standard_times$class)) {
      stop('enter_length must be given in the same class as time', call. = FALSE)
    }
    as.numeric(params$enter_length)
  }
  exit_length <- if (is.null(params$exit_length)) {
    0
  } else {
    if (!inherits(params$exit_length, standard_times$class)) {
      stop('exit_length must be given in the same class as time', call. = FALSE)
    }
    as.numeric(params$exit_length)
  }
  range <- if (is.null(params$range)) {
    range(unlist(standard_times$times)) + c(-enter_length, exit_length)
  } else {
    if (!inherits(params$range, standard_times$class)) {
      stop('range must be given in the same class as time', call. = FALSE)
    }
    as.numeric(params$range)
  }
  full_length <- diff(range)
  frames <- lapply(standard_times$times, function(x) {
    if (is.null(x)) return(numeric())
    frame <- round((params$nframes - 1) * (x - range[1])/full_length) + 1
    nc <- nchar(max(frame))
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
