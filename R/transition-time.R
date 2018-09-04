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
#' @param range The time range to animate. If `NULL` it will be set to the range
#' of `time`
#'
#' @section Label variables:
#' `transition_time` makes the following variables available for string
#' literal interpretation:
#'
#' - **frame_time** gives the time that the current frame corresponds to
#'
#' @family transitions
#'
#' @examples
#'
#' p <- ggplot(airquality, aes(Day, Temp)) +
#'   geom_line(color = 'red', size = 1) +
#'   transition_time(Month)
#'
#' # animate(p)
#'
#' @export
transition_time <- function(time, range = NULL) {
  time_quo <- enquo(time)
  ggproto(NULL, TransitionTime,
          params = list(
            time_quo = time_quo,
            range = range
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
    times <- get_times(data, params$time_quo, params$nframes, params$range)
    params$row_id <- times$values
    params$frame_info <- data.frame(frame_time = times$frame_time)
    params
  },
  expand_panel = function(self, data, type, id, match, ease, enter, exit, params, layer_index) {
    split_panel <- stri_match(data$group, regex = '^(.+)<(.+)>(.*)$')
    if (is.na(split_panel[1])) return(data)
    data$group <- paste0(split_panel[, 2], split_panel[, 4])
    time <- as.integer(split_panel[, 3])
    states <- split(data, time)
    times <- as.integer(names(states))
    nframes <- diff(times)
    nframes[1] <- nframes[1] + 1

    if (times[1] <= 1) {
      all_frames <- states[[1]]
      states <- states[-1]
    } else {
      all_frames <- data[0, , drop = FALSE]
      nframes <- c(times[1] - 1, nframes)
    }
    if (times[length(times)] < params$nframes) {
      states <- c(states, list(data[0, , drop = FALSE]))
      nframes <- c(nframes, params$nframes - times[length(times)])
    }

    for (i in seq_along(states)) {
      all_frames <- switch(
        type,
        point = tween_state(all_frames, states[[i]], ease, nframes[i], !!id, enter, exit),
        path = tween_path(all_frames, states[[i]], ease, nframes[i], !!id, enter, exit, match),
        polygon = tween_polygon(all_frames, states[[i]], ease, nframes[i], !!id, enter, exit, match),
        sf = tween_sf(all_frames, states[[i]], ease, nframes[i], !!id, enter, exit),
        stop("Unknown layer type", call. = FALSE)
      )
    }
    true_frame <- seq(times[1], times[length(times)])
    all_frames <- all_frames[all_frames$.frame %in% which(true_frame > 0 & true_frame <= params$nframes), , drop = FALSE]
    all_frames$.frame <- all_frames$.frame - min(all_frames$.frame) + 1
    all_frames$group <- paste0(all_frames$group, '<', all_frames$.frame, '>')
    all_frames$.frame <- NULL
    all_frames
  }
)


# HELPERS -----------------------------------------------------------------

get_times <- function(data, var, nframes, range) {
  times <- lapply(data, safe_eval, expr = var)
  times <- standardise_times(times, 'time')
  time_class <- times$class
  times <- times$times
  if (is.null(range)) {
    range <- range(unlist(times))
  } else {
    if (!inherits(range, time_class)) {
      stop('range must be given in the same class as time', call. = FALSE)
    }
    range <- as.numeric(range)
  }
  times <- lapply(times, function(v) {
    if (is.null(v)) return(integer())
    round(1 + (nframes - 1) * (v - range[1]) / diff(range))
  })
  frame_time <- seq(range[1], range[2], length.out = nframes)
  frame_time <- recast_times(frame_time, time_class)
  list(values = times, frame_time = frame_time)
}
