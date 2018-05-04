Shadow <- ggproto('Shadow', NULL,
  merge = FALSE,
  params = list(),
  setup_params = function(self, data, params) {
    params
  },
  get_frames = function(self, params, i) {
    integer(0)
  },
  prepare_shadow = function(self, shadow, params) {
    lapply(shadow, do.call, what = rbind)
  },
  prepare_frame_data = function(self, data, shadow, params) {
    Map(function(d, s) {
      rbind(s, d)
    }, d = data, s = shadow)
  }
)
#' @importFrom ggplot2 ggplot_add
#' @export
ggplot_add.Shadow <- function(object, plot, objectname) {
  plot <- as.gganim(plot)
  plot$shadow <- object
  plot
}
