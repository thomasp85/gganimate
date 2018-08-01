#' Keep all data constant across the animation
#'
#' @family transitions
#'
#' @export
transition_null <- function() {
  ggproto(NULL, TransitionNull)
}
#' @rdname gganimate-ggproto
#' @format NULL
#' @usage NULL
#' @export
#' @importFrom ggplot2 ggproto
TransitionNull <- ggproto('TransitionNull', Transition,
  expand_data = function(self, data, type, id, match, ease, enter, exit, params, layer_index) {
    data
  }
)
