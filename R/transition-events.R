#' Transition individual events in and out
#'
#' This transition treats each visual element as an event in time and allows you
#' to control the duration and enter/exit length individually for each event.
#'
#' @param start,end The unquoted expression giving the start and end time of
#' each event. If `end`is `NULL` the event will be treated as having no duration.
#' @param range The range the animation should span. Defaults to the range of
#' the events from they enter to they have exited.
#' @param enter_length,exit_length The unquoted expression giving the length to
#' be used for enter and exit for each event.
#'
#' @section Label variables:
#' `transition_components` makes the following variables available for string
#' literal interpretation, in addition to the general ones provided by
#' [animate()]:
#'
#' - **frame_time** gives the time that the current frame corresponds to
#'
#' @section Object permanence:
#' `transition_events` does not link rows across data to the same graphic
#' element, so elements will be defined uniquely by each row and its specific
#' start, end, enter and exit.
#'
#' @inheritSection transition_states Computed Variables
#'
#' @family transitions
#'
#' @importFrom rlang enquo quo_is_null quo
#' @importFrom ggplot2 ggproto
#' @export
#'
#' @examples
#' data <- data.frame(
#'   x = 1:10,
#'   y = runif(10),
#'   begin = runif(10, 1, 100),
#'   length = runif(10, 5, 20),
#'   enter = runif(10, 5, 10),
#'   exit = runif(10, 5, 10)
#' )
#'
#' anim <- ggplot(data, aes(x, y)) +
#'   geom_col() +
#'   transition_events(start = begin,
#'                     end = begin + length,
#'                     enter_length = enter,
#'                     exit_length = exit) +
#'  enter_grow() +
#'  exit_drift(x_mod = 11) +
#'  exit_fade()
transition_events <- function(start, end = NULL, range = NULL, enter_length = NULL, exit_length = NULL) {
  start_quo <- enquo(start)
  end_quo <- enquo(end)
  enter_length_quo <- enquo(enter_length)
  exit_length_quo <- enquo(exit_length)
  require_quo(start_quo, 'start')
  ggproto(NULL, TransitionEvents,
    params = list(
      start_quo = start_quo,
      end_quo = end_quo,
      range = range,
      enter_length_quo = enter_length_quo,
      exit_length_quo = exit_length_quo
    )
  )
}
#' @rdname gganimate-ggproto
#' @format NULL
#' @usage NULL
#' @export
#' @importFrom ggplot2 ggproto
#' @importFrom stringi stri_match
#' @importFrom tweenr tween_events
TransitionEvents <- ggproto('TransitionEvents', Transition,
  mapping = '(.+?)_(.*?)_(.*?)_(.*?)',
  var_names = c('start', 'end', 'enter_length', 'exit_length'),
  setup_params = function(self, data, params) {
    params$start <- get_row_event(data, params$start_quo, 'start')
    time_class <- if (is_placeholder(params$start)) NULL else params$start$class
    params$end <- get_row_event(data, params$end_quo, 'end', time_class)
    params$enter_length <- get_row_event(data, params$enter_length_quo, 'enter_length', time_class)
    params$exit_length <- get_row_event(data, params$exit_length_quo, 'exit_length', time_class)
    params$require_stat <- is_placeholder(params$start) || is_placeholder(params$end) || is_placeholder(params$enter_length) || is_placeholder(params$exit_length)
    static = lengths(params$start$values) == 0
    params$row_id <- Map(function(st, end, en, ex, s) if (s) character(0) else paste(st, end, en, ex, sep = '_'),
                         st = params$start$values, end = params$end$values, en = params$enter_length$values, ex = params$exit_length$values, s = static)
    params
  },
  setup_params2 = function(self, data, params, row_vars) {
    late_start <- FALSE
    if (is_placeholder(params$start)) {
      params$start <- get_row_event(data, params$start_quo, 'start', after = TRUE)
      late_start <- TRUE
    } else {
      params$start$values <- lapply(row_vars$start, as.numeric)
    }
    time_class <- params$start$class
    if (is_placeholder(params$end)) {
      params$end <- get_row_event(data, params$end_quo, 'end', time_class, after = TRUE)
    } else {
      params$end$values <- lapply(row_vars$end, as.numeric)
    }
    if (is_placeholder(params$enter_length)) {
      params$enter_length <- get_row_event(data, params$enter_length_quo, 'enter_length', time_class, after = TRUE)
    } else {
      params$enter_length$values <- lapply(row_vars$enter_length, as.numeric)
    }
    if (is_placeholder(params$exit_length)) {
      params$exit_length <- get_row_event(data, params$exit_length_quo, 'exit_length', time_class, after = TRUE)
    } else {
      params$exit_length$values <- lapply(row_vars$exit_length, as.numeric)
    }
    times <- recast_event_times(params$start, params$end, params$enter_length, params$exit_length)

    range <- if (is.null(params$range)) {
      low <- min(unlist(Map(function(start, enter) {
        start - (if (length(enter) == 0) 0 else enter)
      }, start = times$start$values, enter = times$enter_length$values)))
      high <- max(unlist(Map(function(start, end, exit) {
        (if (length(end) == 0) start else end) + (if (length(exit) == 0) 0 else exit)
      }, start = times$start$values, end = times$end$values, exit = times$exit_length$values)))
      range  <- c(low, high)
    } else {
      if (!inherits(params$range, time_class)) {
        cli::cli_abort('{.arg range} must be given in the same class as {.field time}')
      }
      as.numeric(params$range)
    }
    full_length <- diff(range)
    frame_time <- recast_times(
      seq(range[1], range[2], length.out = params$nframes),
      time_class
    )

    frame_length <- full_length / params$nframes
    start <- lapply(times$start$values, function(x) {
      round((params$nframes - 1) * (x - range[1])/full_length) + 1
    })
    end <- lapply(times$end$values, function(x) {
      if (length(x) == 0) return(numeric())
      round((params$nframes - 1) * (x - range[1])/full_length) + 1
    })
    enter_length <- lapply(times$enter_length$values, function(x) {
      if (length(x) == 0) return(numeric())
      round(x / frame_length)
    })
    exit_length <- lapply(times$exit_length$values, function(x) {
      if (length(x) == 0) return(numeric())
      round(x / frame_length)
    })

    params$range <- range
    params$frame_time <- frame_time
    static = lengths(start) == 0
    params$row_id <- Map(function(st, end, en, ex, s) if (s) character(0) else paste(st, end, en, ex, sep = '_'),
                         st = start, end = end, en = enter_length, ex = exit_length, s = static)
    params$frame_info <- data_frame0(
      frame_time = frame_time
    )
    params$nframes <- nrow(params$frame_info)
    params
  },
  expand_panel = function(self, data, type, id, match, ease, enter, exit, params, layer_index) {
    row_vars <- self$get_row_vars(data)
    if (is.null(row_vars)) return(data)
    data$group <- paste0(row_vars$before, row_vars$after)
    start <- as.integer(row_vars$start)
    end <- as.integer(row_vars$end)
    if (is.na(end[1])) end <- NULL
    enter_length <- as.integer(row_vars$enter_length)
    if (is.na(enter_length[1])) enter_length <- NULL
    exit_length <- as.integer(row_vars$exit_length)
    if (is.na(exit_length[1])) exit_length <- NULL
    all_frames <- switch(
      type,
      point = tween_events(data, ease, params$nframes, !!start, !!end, c(1, params$nframes), enter, exit, !!enter_length, !!exit_length),
      cli::cli_abort('{type} layers not currently supported by {.fun transition_events}')
    )
    all_frames$group <- paste0(all_frames$group, '<', all_frames$.frame, '>')
    all_frames$.frame <- NULL
    all_frames
  }
)
get_row_event <- function(data, quo, name, to_class = NULL, after = FALSE) {
  if (!after && require_stat(rlang::quo_get_expr(quo))) {
    return(eval_placeholder(data))
  }
  row_event <- lapply(data, safe_eval, expr = quo)
  row_event <- standardise_times(row_event, name, to_class)
  names(row_event)[names(row_event) == 'times'] <- 'values'
  if (length(row_event$class) != 0) {
    row_event$range <- range(unlist(row_event$values))
    if (diff(row_event$range) != 0) {
      row_event$values <- lapply(row_event$values, function(t) (t - row_event$range[1])/diff(row_event$range))
    }
  }
  row_event
}
recast_event_times <- function(start, end, enter_length, exit_length, late_start) {
  start$values <- lapply(start$values, rescale_to, range = start$range)
  end$values <- lapply(end$values, rescale_to, range = end$range)
  enter_length$values <- lapply(enter_length$values, rescale_to, range = enter_length$range)
  exit_length$values <- lapply(exit_length$values, rescale_to, range = exit_length$range)
  real_class <- start$class
  if (real_class != 'difftime' && real_class != 'hms') {
    if (enter_length$class == 'difftime' || enter_length$class == 'hms') {
      enter_length <- standardise_times(lapply(enter_length$values, recast_times, enter_length$class), 'enter_length', real_class)
      names(enter_length)[names(enter_length) == 'times'] <- 'values'
    }
    if (exit_length$class == 'difftime' || exit_length$class == 'hms') {
      exit_length <- standardise_times(lapply(exit_length$values, recast_times, exit_length$class), 'exit_length', real_class)
      names(exit_length)[names(exit_length) == 'times'] <- 'values'
    }
  }
  if (vctrs::vec_unique_count(c(start$class, end$class, enter_length$class, exit_length$class)) > 1) {
    cli::cli_abort('{.arg {c("start", "end", "enter_length", "exit_length")}} must have the same class')
  }
  list(
    start = start,
    end = end,
    enter_length = enter_length,
    exit_length = exit_length
  )
}
rescale_to <- function(x, range) {
  if (is.null(range)) return(NULL)
  if (diff(range) == 0) return(rep(range[1], length(x)))
  (x * diff(range)) + range[1]
}
