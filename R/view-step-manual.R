#' @include view-step.R
NULL
#' @rdname view_step
#'
#' @param xmin,xmax,ymin,ymax Vectors of even length defining the boundaries of
#' the different views to go through
#'
#' @export
#' @importFrom ggplot2 ggproto
view_step_manual <- function(pause_length = 1, step_length = 1, xmin, xmax, ymin, ymax,
                      delay = 0, ease = 'cubic-in-out', wrap = TRUE,
                      pause_first = FALSE, fixed_x = FALSE, fixed_y = FALSE,
                      exclude_layer = NULL, aspect_ratio = 1) {
  ggproto(NULL, ViewStepManual,
          fixed_lim = list(x = fixed_x, y = fixed_y),
          exclude_layer = exclude_layer,
          aspect_ratio = aspect_ratio,
          params = list(
            pause_length = pause_length,
            step_length = step_length,
            windows = data_frame0(
              xmin = xmin,
              xmax = xmax,
              ymin = ymin,
              ymax = ymax
            ),
            delay = delay,
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
#' @importFrom tweenr keep_state
ViewStepManual <- ggproto('ViewStepManual', ViewStep,
  setup_params = function(self, data, params) {
    nsteps <- nrow(params$windows)
    step_length <- rep(params$step_length, length.out = nsteps)
    pause_length <- rep(params$pause_length, length.out = nsteps)
    if (!params$pause_first) {
      pause_length <- c(0, pause_length)
      step_length <- c(step_length, 0)
      if (!params$wrap) pause_length[length(pause_length)] <- 0
    } else if (!params$wrap) {
      step_length[length(step_length)] <- 0
    }
    params$step_length <- step_length
    params$pause_length <- pause_length
    params
  },
  train = function(self, data, params) {
    nframes <- params$nframes
    if (params$wrap) nframes <- nframes + 1
    frames <- distribute_frames(params$pause_length, params$step_length, nframes)
    params$windows <- vec_rbind0(
      params$windows,
      params$windows[rep(1, length(frames$static_length) - nrow(params$windows) + 1), , drop = FALSE]
    )
    frame_ranges <- params$windows[1, , drop = FALSE]

    for (i in seq_len(nrow(params$windows) - 1)) {
      if (frames$static_length[i] != 0) {
        frame_ranges <- keep_state(frame_ranges, frames$static_length[i])
      }
      if (frames$transition_length[i] != 0) {
        frame_ranges <- self$window_transition(frame_ranges, params$windows[i + 1, , drop = FALSE], frames$transition_length[i], params)
      }
    }
    frame_ranges <- frame_ranges[seq_len(params$nframes), , drop = FALSE]
    frame_ranges$.frame <- (seq_len(nrow(frame_ranges)) + round(params$delay * frames$mod)) %% params$nframes
    frame_ranges <- frame_ranges[order(frame_ranges$.frame), ]
    params$frame_ranges <- frame_ranges
    params
  }
)
