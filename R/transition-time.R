#' @include transition-manual.R
NULL

#' Transition through distinct states in time
#'
#' This is a variant of [transition_states()] that is intended for data where
#' the states are representing specific point in time. The transition length
#' between the states will be set to correspond to the actual time difference
#' between them.
#'
#' @param time An unquoted expression giving the time, and thus state
#' membership, of each observation.
#'
#' @section Label variables:
#' `transition_time` makes the following variables available for string
#' literal interpretation:
#'
#' - **frame_time** gives the time that the current frame corresponds to
#'
#' @family transitions
#'
#' @export
transition_time <- function(time) {
  time_quo <- enquo(time)
  ggproto(NULL, TransitionTime,
          params = list(
            time_quo = time_quo,
            start_pause = start_pause,
            end_pause = end_pause
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
#' @importFrom transformr tween_path tween_polygon tween_sf
TransitionTime <- ggproto('TransitionTime', TransitionManual,
  setup_params = function(self, data, params) {
    times <- get_times(data, params$time_quo, params$nframes, params$end_pause)
    params$row_id <- times$values
    params$frame_info <- data.frame(frame_time = times$frame_time)
    params
  },
  expand_data = function(self, data, type, ease, enter, exit, params, layer_index) {
    Map(function(d, t, en, ex, es) {
      split_panel <- stri_match(d$group, regex = '^(.+)_(.+)$')
      if (is.na(split_panel[1])) return(d)
      d$group <- as.integer(split_panel[, 2])
      time <- as.integer(split_panel[, 3])
      states <- split(d, time)
      times <- as.integer(names(states))
      nframes <- diff(times)
      id <- if (d$group[1] == -1) NULL else 'group'

      if (times[1] == 1) {
        all_frames <- states[[1]]
        states <- states[-1]
      } else {
        all_frames <- d[0, , drop = FALSE]
        nframes <- c(times[1] - 1, nframes)
      }
      if (times[length(times)] != params$nframes) {
        states <- c(states, list(d[0, , drop = FALSE]))
        nframes <- c(nframes, params$nframes - times[length(times)])
      }

      for (i in seq_along(states)) {
        all_frames <- switch(
          t,
          point = tween_state(all_frames, states[[i]], es, nframes[i], id, en, ex),
          path = tween_path(all_frames, states[[i]], es, nframes[i], id, en, ex),
          polygon = tween_polygon(all_frames, states[[i]], es, nframes[i], id, en, ex),
          sf = tween_sf(all_frames, states[[i]], es, nframes[i], id, en, ex),
          stop("Unknown layer type", call. = FALSE)
        )
      }
      all_frames$group <- paste0(all_frames$group, '_', all_frames$.frame)
      all_frames$.frame <- NULL
      all_frames
    }, d = data, t = type, en = enter, ex = exit, es = ease)
  }
)


# HELPERS -----------------------------------------------------------------

get_times <- function(data, var, nframes, start_pause, end_pause) {
  times <- lapply(data, safe_eval, expr = var)
  times <- standardise_times(times, 'time')
  time_class <- times$class
  times <- times$times
  time_range <- range(unlist(times))
  start <- time_range[1]
  end <- time_range[2]
  midpoints <- seq(start, end, length.out = 100)
  breaks <- c(-Inf, midpoints[-1] - diff(midpoints)/2, Inf)
  times[lengths(times) != vapply(data, nrow, integer(1))] <- list(NULL)
  times <- lapply(times, function(v) {
    if (is.null(v)) return(integer())
    cut(v, breaks, labels = FALSE)
  })
  frame_time <- seq(start, end, length.out = nframes)
  frame_time <- recast_times(frame_time, time_class)
  list(values = times, frame_time = frame_time)
}
