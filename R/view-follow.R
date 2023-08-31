#' Let the view follow the data in each frame
#'
#' This view will set the panels to include the data present in the frame.
#'
#' @param fixed_x,fixed_y Either a logical indicating if the dimension should
#' not be modified by the view, or a numeric vector giving the lower and upper
#' bounds of the dimension. For the latter, an `NA` value will be substituted
#' for whatever is calculated by the view (e.g. `fixed_x = c(0, NA)`) will fix
#' the minimum x value to 0 and let the view calculate the upper bound.
#' @param exclude_layer Integer vector of layer indices that should be ignored
#' when calculating the view
#' @param aspect_ratio If the coord is fixed, ensure that the view matches the
#' given aspect ratio. Will override anything given in `fixed_x`/`fixed_y`
#'
#' @family views
#'
#' @importFrom ggplot2 ggproto
#' @export
#'
#' @examples
#' anim <- ggplot(iris, aes(Sepal.Length, Sepal.Width)) +
#'   geom_point() +
#'   labs(title = "{closest_state}") +
#'   transition_states(Species, transition_length = 4, state_length = 1) +
#'   view_follow()
#'
#' # Fixing a dimension can be done in general
#' anim1 <- ggplot(iris, aes(Sepal.Length, Sepal.Width)) +
#'   geom_point() +
#'   labs(title = "{closest_state}") +
#'   transition_states(Species, transition_length = 4, state_length = 1) +
#'   view_follow(fixed_x = TRUE)
#'
#' # ...or just for one side of the range
#' anim1 <- ggplot(iris, aes(Sepal.Length, Sepal.Width)) +
#'   geom_point() +
#'   labs(title = "{closest_state}") +
#'   transition_states(Species, transition_length = 4, state_length = 1) +
#'   view_follow(fixed_x = c(4, NA), fixed_y = c(2, NA))
#'
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
      cli::cli_abort('{.fun view_follow} does not support polar coordinates')
    }
    ranges <- self$get_ranges(plot$data, params)
    ranges <- ranges[!seq_along(ranges) %in% params$excluded_layers]
    x_range <- range(inf.omit(unlist(lapply(ranges, `[[`, 'x'))))
    y_range <- range(inf.omit(unlist(lapply(ranges, `[[`, 'y'))))
    if (!is.null(plot$layout$panel_scales_x[[1]]$trans)) {
      x_range <- plot$layout$panel_scales_x[[1]]$trans$inverse(x_range)
    }
    if (!is.null(plot$layout$panel_scales_y[[1]]$trans)) {
      y_range <- plot$layout$panel_scales_y[[1]]$trans$inverse(y_range)
    }

    self$reset_limits(plot, x_range, y_range)
  }
)

inf.omit <- function(x) x[is.finite(x)]
