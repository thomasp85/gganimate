#' @include transition-manual.R
NULL

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
#' literal interpretation:
#'
#' - **frame_time** gives the time that the current frame corresponds to
#'
#' @family transitions
#'
#' @export
#' @importFrom rlang enquo quo_is_null quo
#' @importFrom ggplot2 ggproto
transition_events <- function(start, end = NULL, range = NULL, enter_length = NULL, exit_length = NULL) {
  start_quo <- enquo(start)
  end_quo <- enquo(end)
  enter_length_quo <- enquo(enter_length)
  exit_length_quo <- enquo(exit_length)
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
TransitionEvents <- ggproto('TransitionEvents', TransitionManual,
  setup_params = function(self, data, params) {
    data_info <- event_info(data, params)

    params$range <- data_info$range
    params$frame_time <- data_info$frame_time
    params$row_id <- Map(function(st, end, en, ex, s) if (s) character(0) else paste(st, end, en, ex, sep = '-'),
                         st = data_info$start_frame, end = data_info$end_frame,
                         en = data_info$enter_length, ex = data_info$exit_length,
                         s = data_info$static)
    params$frame_info <- data.frame(
      frame_time = data_info$frame_time
    )
    params$nframes <- nrow(params$frame_info)
    params
  },
  expand_panel = function(self, data, type, id, match, ease, enter, exit, params, layer_index) {
    split_panel <- stri_match(data$group, regex = '^(.+)<(.+?)-(.*?)-(.*?)-(.*?)>(.*)$')
    if (is.na(split_panel[1])) return(data)
    data$group <- paste0(split_panel[, 2], split_panel[, 7])
    start <- as.integer(split_panel[, 3])
    end <- as.integer(split_panel[, 4])
    if (is.na(end[1])) end <- NULL
    enter_length <- as.integer(split_panel[, 5])
    if (is.na(enter_length[1])) enter_length <- NULL
    exit_length <- as.integer(split_panel[, 6])
    if (is.na(exit_length[1])) exit_length <- NULL
    all_frames <- switch(
      type,
      point = tween_events(data, ease, params$nframes, !!start, !!end, params$range, enter, exit, !!enter_length, !!exit_length),
      #path = tween_path(all_frames, next_state, es, params$transition_length[i], id, en, ex),
      #polygon = tween_polygon(all_frames, next_state, es, params$transition_length[i], id, en, ex),
      #sf = tween_sf(all_frames, next_state, es, params$transition_length[i], id, en, ex),
      stop("Unknown layer type", call. = FALSE)
    )
    all_frames$group <- paste0(all_frames$group, '<', all_frames$.frame, '>')
    all_frames$.frame <- NULL
    all_frames
  }
)

event_info <- function(data, params) {
  row_start <- lapply(data, safe_eval, expr = params$start_quo)
  row_end <- lapply(data, safe_eval, expr = params$end_quo)
  row_enter_length <- lapply(data, safe_eval, expr = params$enter_length_quo)
  row_exit_length <- lapply(data, safe_eval, expr = params$exit_length_quo)

  static_layer <- unlist(Map(function(data, start) {
    length(start) != 1 && length(start) != nrow(data)
  }, data = data, start = row_start))
  if (all(static_layer)) {
    stop('At least one layer must be in transition', call. = FALSE)
  }
  row_start[static_layer] <- list(numeric(0))
  row_end[static_layer] <- list(numeric(0))
  row_enter_length[static_layer] <- list(numeric(0))
  row_exit_length[static_layer] <- list(numeric(0))
  row_start <- standardise_times(row_start, 'start')
  time_class <- row_start$class
  row_start <- row_start$times
  row_end <- standardise_times(row_end, 'end', time_class)$times
  row_enter_length <- standardise_times(row_enter_length, 'enter_length', time_class)$times
  row_exit_length <- standardise_times(row_exit_length, 'exit_length', time_class)$times

  range <- if (is.null(params$range)) {
    low <- min(unlist(Map(function(start, enter) {
      start - (if (length(enter) == 0) 0 else enter)
    }, start = row_start, enter = row_enter_length)))
    high <- max(unlist(Map(function(start, end, exit) {
      (if (length(end) == 0) start else end) + (if (length(exit) == 0) 0 else exit)
    }, start = row_start, end = row_end, exit = row_exit_length)))
    range  <- c(low, high)
  } else {
    if (!inherits(params$range, time_class)) {
      stop('range must be given in the same class as time', call. = FALSE)
    }
    as.numeric(params$range)
  }
  full_length <- diff(range)
  frame_time <- recast_times(
    seq(range[1], range[2], length.out = params$nframes),
    time_class
  )
  frame_length <- full_length / params$nframes
  start_frame <- lapply(row_start, function(x) {
    round((params$nframes - 1) * (x - range[1])/full_length) + 1
  })
  end_frame <- lapply(row_end, function(x) {
    if (length(x) == 0) return(numeric())
    round((params$nframes - 1) * (x - range[1])/full_length) + 1
  })
  enter_length <- lapply(row_enter_length, function(x) {
    if (length(x) == 0) return(numeric())
    round(x / frame_length)
  })
  exit_length <- lapply(row_exit_length, function(x) {
    if (length(x) == 0) return(numeric())
    round(x / frame_length)
  })

  list(
    start_frame = start_frame,
    end_frame = end_frame,
    enter_length = enter_length,
    exit_length = exit_length,
    frame_time = frame_time,
    static = static_layer
  )
}
