#' @rdname gganimate-ggproto
#' @format NULL
#' @usage NULL
#' @export
#' @importFrom ggplot2 ggproto
View <- ggproto('View', NULL,
  exclude_layer = numeric(0),
  fixed_lim = list(x = FALSE, y = FALSE),
  setup_params = function(self, data, params) {
    params
  },
  train = function(self, data, params) {
    params
  },
  set_view = function(self, plot, params, i) {
    plot
  },
  reset_limits = function(self, plot, xlim, ylim) {
    if (is.logical(self$fixed_lim$x)) {
      if (self$fixed_lim$x) xlim <- plot$layout$coord$limits$x
    } else {
      xlim[!is.na(self$fixed_lim$x)] <- self$fixed_lim$x[!is.na(self$fixed_lim$x)]
    }
    if (is.logical(self$fixed_lim$y)) {
      if (self$fixed_lim$y) ylim <- plot$layout$coord$limits$y
    } else {
      ylim[!is.na(self$fixed_lim$y)] <- self$fixed_lim$y[!is.na(self$fixed_lim$y)]
    }
    plot$layout$coord$limits$x <- xlim
    plot$layout$coord$limits$y <- ylim
    plot$layout$setup_panel_params()
    plot
  },
  get_ranges = function(self, data) {
    lapply(data[!seq_along(data) %in% self$exclude_layer], function(d) {
      if ('geometry' %in% names(d)) {
        bbox <- sf::st_bbox(d$geometry)
        list(x = c(bbox$xmin, bbox$xmax), y = c(bbox$ymin, bbox$ymax))
      } else {
        x <- unlist(d[names(d) %in% x_aes])
        y <- unlist(d[names(d) %in% y_aes])
        list(x = range(x), y = range(y))
      }
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
