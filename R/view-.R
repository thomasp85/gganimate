#' @rdname gganimate-ggproto
#' @format NULL
#' @usage NULL
#' @export
#' @importFrom ggplot2 ggproto
View <- ggproto('View', NULL,
  setup_params = function(self, data, params) {
    params
  },
  train = function(self, data, params) {
    params
  },
  set_view = function(self, plot, params, i) {
    plot
  },
  reset_limits = function(plot, xlim, ylim) {
    plot$layout$coord$limits$x <- xlim
    plot$layout$coord$limits$y <- ylim
    plot$layout$setup_panel_params()
    plot
  },
  get_ranges = function(data) {
    lapply(data, function(d) {
      x <- unlist(d[names(d) %in% x_aes])
      y <- unlist(d[names(d) %in% y_aes])
      list(x = range(x), y = range(y))
    })
  },
  add_label_vars = function(self, var, i, params, plot) {
    var
  }
)
#' @importFrom ggplot2 ggplot_add
#' @export
ggplot_add.View <- function(object, plot, objectname) {
  plot <- as.gganim(plot)
  plot$view <- object
  plot
}
