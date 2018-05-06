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
  get_frames = function(self, params, i) {
    integer(0)
  },
  prepare_shadow = function(self, shadow, params) {
    lapply(shadow, do.call, what = rbind)
  },
  prepare_frame_data = function(self, data, shadow, params) {
    Map(function(d, s, e) {
      if (e) return(d[[1]])
      rbind(s, d[[1]])
    }, d = data, s = shadow, e = seq_along(data) %in% self$exclude_layer)
  }
)
#' @importFrom ggplot2 ggplot_add
#' @export
ggplot_add.Shadow <- function(object, plot, objectname) {
  plot <- as.gganim(plot)
  plot$shadow <- object
  plot
}
