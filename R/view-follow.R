#' Let the view follow the data in each frame
#'
#' This view will set the panels to include the data present in the frame.
#'
#' @param exclude_layer Integer vector of layer indices that should be ignored
#' when calculating the view
#' @param fixed_x,fixed_y Either a logical indicating if the dimension should
#' not be modified by the view, or a numeric vector giving the lower and upper
#' bounds of the dimension. For the latter, an `NA` value will be substituted
#' for whatever is calculated by the view (e.g. `fixed_x = c(0, NA)`) will fix
#' the minimum x value to 0 and let the view calculate the upper bound.
#'
#' @family views
#'
#' @export
#' @importFrom ggplot2 ggproto
view_follow <- function(fixed_x = FALSE, fixed_y = FALSE, exclude_layer = NULL) {
  ggproto(NULL, ViewFollow, exclude_layer = exclude_layer,
          fixed_lim = list(x = fixed_x, y = fixed_y))
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
