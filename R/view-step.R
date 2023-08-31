#' Follow the data in steps
#'
#' This view is a bit like [view_follow()] but will not match the data in each
#' frame. Instead it will switch between being static and zoom to the range of
#' the data. It is a great pairing with [transition_states()] as it can move the
#' view while the data is static and then be static while the data moves. The
#' standard version will look at the data present in the calculated frames and
#' set the ranges based on that, while the `_manual` version will allow you to
#' define your own ranges.
#'
#' @param pause_length The relative length the view will be kept static. Will
#' be recycled to match the number of steps
#' @param step_length The relative length the view will use to transition to the
#' new position. Will be recycled to match the number of steps
#' @param nsteps The number of steps. If `NULL` it will be set to the max length
#' of `pause_length` or `step_length`
#' @param look_ahead A relative length to look ahead in the animation to get the
#' new zoom area. Allow the view to zoom to where the data will be
#' @param delay A relative length to switch the view back and forth relative to
#' the actual frames. E.g. if delay is calculated to 5 frames, frame 6 will get
#' the view intended for frame 1.
#' @param include Should the steps include both the start and end frame range
#' @param ease The easing function used for the step
#' @param wrap As in [transition_states()]. Should the view wrap around and zoom
#' back to the first state.
#' @param pause_first Should the view start with a pause. The default is to
#' start with a step so that it is aligned to the static period in
#' [transition_states()]
#' @inheritParams view_follow
#'
#' @family views
#'
#' @importFrom ggplot2 ggproto
#' @export
#'
#' @examples
#' anim <- ggplot(iris, aes(Petal.Length, Petal.Width)) +
#'   geom_point() +
#'   transition_states(Species, transition_length = 2, state_length = 1) +
#'   view_step(pause_length = 2, step_length = 1, nsteps = 3)
#'
#' # Default is to include the data from the two states you're stepping between
#' # but this can be turned off
#' anim <- ggplot(iris, aes(Petal.Length, Petal.Width)) +
#'   geom_point() +
#'   transition_states(Species, transition_length = 2, state_length = 1) +
#'   view_step(pause_length = 2, step_length = 1, nsteps = 3, include = FALSE)
#'
#' # Default is to work off-beat of transition_states so that view changes while
#' # data is static. Setting pause_first=TRUE changes this
#' anim <- ggplot(iris, aes(Petal.Length, Petal.Width)) +
#'   geom_point() +
#'   transition_states(Species, transition_length = 2, state_length = 1) +
#'   view_step(pause_length = 1, step_length = 2, nsteps = 3, pause_first = TRUE)
#'
#' # If the transition doesn't wrap, then the view shouldn't either
#' anim <- ggplot(iris, aes(Petal.Length, Petal.Width)) +
#'   geom_point() +
#'   transition_states(Species, transition_length = 2, state_length = 1, wrap = FALSE) +
#'   view_step(pause_length = 2, step_length = 1, nsteps = 3, wrap = FALSE)
#'
view_step <- function(pause_length = 1, step_length = 1, nsteps = NULL, look_ahead = pause_length,
                      delay = 0, include = TRUE, ease = 'cubic-in-out', wrap = TRUE,
                      pause_first = FALSE, fixed_x = FALSE, fixed_y = FALSE,
                      exclude_layer = NULL, aspect_ratio = 1) {
  ggproto(NULL, ViewStep,
    fixed_lim = list(x = fixed_x, y = fixed_y),
    exclude_layer = exclude_layer,
    aspect_ratio = aspect_ratio,
    params = list(
      pause_length = pause_length,
      step_length = step_length,
      nsteps = nsteps,
      look_ahead = look_ahead,
      delay = delay,
      include = include,
      ease = ease,
      wrap = wrap,
      pause_first = pause_first
    )
  )
}

#' @rdname gganimate-ggproto
#' @format NULL
#' @usage NULL
#' @export
#' @importFrom ggplot2 ggproto
#' @importFrom tweenr keep_state tween_state
ViewStep <- ggproto('ViewStep', View,
  setup_params = function(self, data, params) {
    nsteps <- params$nstep %||% max(length(params$step_length), length(params$pause_length))
    step_length <- rep(params$step_length, length.out = nsteps)
    pause_length <- rep(params$pause_length, length.out = nsteps)
    look_ahead <- rep(params$look_ahead, length.out = nsteps)
    if (!params$pause_first) {
      pause_length <- c(0, pause_length)
      step_length <- c(step_length, 0)
      look_ahead <- c(look_ahead, look_ahead[1])
      if (!params$wrap) pause_length[length(pause_length)] <- 0
    } else if (!params$wrap) {
      step_length[length(step_length)] <- 0
    }
    params$step_length <- step_length
    params$pause_length <- pause_length
    params$look_ahead <- look_ahead
    params
  },
  train = function(self, data, params) {
    nframes <- params$nframes
    if (params$wrap) nframes <- nframes + 1
    frames <- distribute_frames(params$pause_length, params$step_length, nframes)
    if (!params$pause_first) {
      frames$transition_length[length(frames$transition_length)] <- frames$transition_length[1]
    }
    look_ahead <- round(params$look_ahead * frames$mod)
    breaks <- cumsum(frames$static_length + frames$transition_length) + look_ahead
    if (params$wrap) {
      breaks <- breaks %% nframes
      breaks[breaks == 0] <- 1L
      breaks <- if (params$pause_first) {
        c(breaks[length(breaks)], breaks)
      } else {
        c(breaks[length(breaks) - 1], breaks)
      }
    } else {
      breaks <- c(1, pmin(breaks, params$nframes))
    }
    data <- data[!seq_along(data) %in% params$excluded_layers]
    windows <- lapply(breaks, function(i) {
      data <- lapply(data, `[[`, i)
      ranges <- self$get_ranges(data, params)
      ranges <- ranges[!seq_along(ranges) %in% params$excluded_layers]
      x_range <- range(unlist(lapply(ranges, `[[`, 'x')))
      y_range <- range(unlist(lapply(ranges, `[[`, 'y')))
      data_frame0(xmin = x_range[1], xmax = x_range[2], ymin = y_range[1], ymax = y_range[2])
    })
    if (params$include) {
      windows <- lapply(seq_along(windows), function(i) {
        if (i == 1) {
          if (!params$wrap) return(windows[[i]])
          else i <- c(length(windows), i)
        } else {
          i <- c(i - 1, i)
        }
        range <- vec_rbind0(!!!windows[i])
        data_frame0(xmin = min(range$xmin), xmax = max(range$xmax), ymin = min(range$ymin), ymax = max(range$ymax))
      })
    }

    frame_ranges <- if (params$wrap) {
      if (params$pause_first) {
        windows[[length(windows)]]
      } else {
        windows[[length(windows) - 1]]
      }
    } else {
      windows[[1]]
    }

    for (i in seq_len(length(windows) - 1)) {
      if (frames$static_length[i] != 0) {
        frame_ranges <- keep_state(frame_ranges, frames$static_length[i])
      }
      if (frames$transition_length[i] != 0) {
        frame_ranges <- self$window_transition(frame_ranges, windows[[i + 1]], frames$transition_length[i], params)
      }
    }
    frame_ranges <- frame_ranges[frame_ranges$.frame <= params$nframes, ]
    frame_ranges$.frame <- (frame_ranges$.frame + round(params$delay * frames$mod)) %% params$nframes
    frame_ranges <- frame_ranges[order(frame_ranges$.frame), ]
    params$frame_ranges <- frame_ranges
    params
  },
  set_view = function(self, plot, params, i) {
    range <- params$frame_ranges[i, ]
    self$reset_limits(plot, c(range$xmin, range$xmax), c(range$ymin, range$ymax))
  },
  window_transition = function(windows, next_window, n, params) {
    tween_state(windows, next_window, params$ease, n)
  }
)
