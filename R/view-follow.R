#' Let the view follow the data in each frame
#'
#' This view will set the panels to include the data present in the frame.
#'
#' @family views
#'
#' @export
#' @importFrom ggplot2 ggproto
view_follow <- function() {
  ggproto(NULL, ViewFollow)
}

#' @rdname gganimate-ggproto
#' @format NULL
#' @usage NULL
#' @export
#' @importFrom ggplot2 ggproto
ViewFollow <- ggproto('ViewFollow', View,
  set_view = function(self, plot, params, i) {
    if (inherits(plot$layout$coord, 'CoordPolar')) {
      stop('This view does not support polar coordinates')
    }
    ranges <- self$get_ranges(plot$data)
    x_range <- range(unlist(lapply(ranges, `[[`, 'x')))
    y_range <- range(unlist(lapply(ranges, `[[`, 'y')))
    self$reset_limits(plot, x_range, y_range)
  }
)
