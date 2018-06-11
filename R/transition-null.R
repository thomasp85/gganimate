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
  finish_data = function(self, data, params, nframes) {
    lapply(data, function(d) {
      rep(list(d), params$nframes)
    })
  }
)
