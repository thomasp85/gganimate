#' @rdname gganimate-ggproto
#' @format NULL
#' @usage NULL
#' @export
#' @importFrom ggplot2 ggproto
Shadow <- ggproto('Shadow', NULL,
  exclude_layer = integer(0),
  params = list(),
  setup_params = function(self, data, params) {
    params
  },
  train = function(self, data, params) {
    params
  },
  get_frames = function(self, params, i) {
    integer(0)
  },
  prepare_shadow = function(self, shadow, params) {
    lapply(shadow, function(x) vec_rbind0(!!!x))
  },
  prepare_frame_data = function(self, data, shadow, params, frame_ind, shadow_ind) {
    Map(function(d, s, e) {
      if (e) return(d[[1]])
      vec_rbind0(s, d[[1]])
    }, d = data, s = shadow, e = seq_along(data) %in% params$excluded_layers)
  }
)
#' @importFrom ggplot2 ggplot_add
#' @export
ggplot_add.Shadow <- function(object, plot, ...) {
  plot <- as.gganim(plot)
  plot$shadow <- object
  plot
}
