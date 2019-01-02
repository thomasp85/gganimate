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
  setup_params = function(self, data, params) {
    params$n_layers <- length(data)
    params
  },
  expand_data = function(self, data, type, id, match, ease, enter, exit, params, layer_index) {
    data
  },
  static_layers = function(self, params) {
    seq_len(params$n_layers)
  },
  unmap_frames = function(self, data, params) {
    data
  },
  remap_frames = function(self, data, params) {
    data
  },
  finish_data = function(self, data, params) {
    lapply(data, list)
  },
  adjust_nframes = function(self, data, params) {
    params$nframes
  },
  get_frame_vars = function(self, params) {
    NULL
  }
)
