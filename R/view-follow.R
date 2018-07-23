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
#' @param aspect_ratio If the coord is fixed, ensure that the view matches the
#' given aspect ratio. Will override anything given in `fixed_x`/`fixed_y`
#'
#' @family views
#'
#' @examples
#'
#' # `view_follow()` can be combined with `transition_states()` to follow
#' # transitions in each frame.
#'
#' p <- ggplot(iris, aes(Sepal.Length, Sepal.Width)) +
#'  geom_point() +
#'  labs(title = "{closest_state}") +
#'  transition_states(Species, transition_length = 4, state_length = 1) +
#'  view_follow()
#'
#' # animate(p)
#'
#' @export
#' @importFrom ggplot2 ggproto
view_follow <- function(fixed_x = FALSE, fixed_y = FALSE, exclude_layer = NULL, aspect_ratio = 1) {
  ggproto(NULL, ViewFollow, exclude_layer = exclude_layer, aspect_ratio = aspect_ratio,
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
    ranges <- self$get_ranges(plot$data, params)
    x_range <- range(unlist(lapply(ranges, `[[`, 'x')))
    y_range <- range(unlist(lapply(ranges, `[[`, 'y')))
    self$reset_limits(plot, x_range, y_range)
  }
)
