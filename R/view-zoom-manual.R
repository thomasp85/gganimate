#' @include view-step-manual.R
#' @include view-zoom.R
NULL

#' @rdname view_zoom
#'
#' @param xmin,xmax,ymin,ymax Vectors of even length defining the boundaries of
#' the different views to go through
#'
#' @export
#' @importFrom ggplot2 ggproto
view_zoom_manual <- function(pause_length, step_length, xmin, xmax, ymin, ymax,
                      delay = 0, wrap = TRUE, pause_first = FALSE,
                      fixed_x = FALSE, fixed_y = FALSE, exclude_layer = NULL, aspect_ratio = 1) {
  ggproto(NULL, ViewZoomManual,
          fixed_lim = list(x = fixed_x, y = fixed_y),
          exclude_layer = exclude_layer,
          aspect_ratio = aspect_ratio,
          params = list(
            pause_length = pause_length,
            step_length = step_length,
            windows = data.frame(
              xmin = xmin,
              xmax = xmax,
              ymin = ymin,
              ymax = ymax
            ),
            delay = delay,
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
ViewZoomManual <- ggproto('ViewZoomManual', ViewStepManual,
  window_transition = transition_window
)
