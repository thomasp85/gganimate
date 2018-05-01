#' @rdname gganimate-ggproto
#' @format NULL
#' @usage NULL
#' @export
#' @importFrom ggplot2 ggproto
Transition <- ggproto('Transition', NULL,
  params = NULL,
  setup_params = function(self, data, params) {
    params
  },
  map_data = function(self, data, params) {
    data
  },
  expand_data = function(self, data, type, ease, enter, exit, params) {
    data
  },
  unmap_frames = function(self, data, params) {
    data
  },
  remap_frames = function(self, data, params) {
    data
  },
  finish_data = function(self, data, params) {
    data
  },
  adjust_nframes = function(slef, data, params, nframes) {
    nframes
  }
)
#' @importFrom ggplot2 ggplot_add
#' @export
ggplot_add.Transition <- function(object, plot, objectname) {
  plot <- as.gganim(plot)
  plot$transition <- object
  plot
}
