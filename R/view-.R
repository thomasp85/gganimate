#' @rdname gganimate-ggproto
#' @format NULL
#' @usage NULL
#' @export
#' @importFrom ggplot2 ggproto
View <- ggproto('View', NULL,
  exclude_layer = numeric(0),
  fixed_lim = list(x = FALSE, y = FALSE),
  aspect_ratio = 1,
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
    if (is_logical(self$fixed_lim$x)) {
      if (self$fixed_lim$x) xlim <- plot$layout$coord$limits$x
    } else {
      xlim[!is.na(self$fixed_lim$x)] <- self$fixed_lim$x[!is.na(self$fixed_lim$x)]
    }
    if (is_logical(self$fixed_lim$y)) {
      if (self$fixed_lim$y) ylim <- plot$layout$coord$limits$y
    } else {
      ylim[!is.na(self$fixed_lim$y)] <- self$fixed_lim$y[!is.na(self$fixed_lim$y)]
    }
    if (!plot$layout$coord$is_free()) {
      width <- diff(xlim)
      height <- diff(ylim)
      current_asp <- if (width == height) 1 else if (height == 0) Inf else width / height
      if (current_asp > self$aspect_ratio) {
        new_height <- width / self$aspect_ratio
        pad <- (new_height - height) / 2
        ylim <- ylim + c(-pad, pad)
      } else if (current_asp < self$aspect_ratio) {
        new_width <- height * self$aspect_ratio
        pad <- (new_width - width) / 2
        xlim <- xlim + c(-pad, pad)
      }
    }
    plot$layout$coord$limits$x <- xlim
    plot$layout$coord$limits$y <- ylim
    plot$layout$setup_panel_params()
    if (inherits(plot$layout$coord, 'CoordFlip')) {
      # We need to do it twice because CoordFlip flips the scales in-place
      plot$layout$setup_panel_params()
    }
    if (inherits(plot$plot$guides, "Guides")) {
      plot$layout$setup_panel_guides(plot$plot$guides, plot$plot$layers)
    }
    plot
  },
  get_ranges = function(self, data, params) {
    lapply(data, function(d) {
      if ('geometry' %in% names(d)) {
        bbox <- sf::st_bbox(d$geometry)
        list(x = c(bbox$xmin, bbox$xmax), y = c(bbox$ymin, bbox$ymax))
      } else {
        x <- unlist(d[names(d) %in% x_aes])
        y <- unlist(d[names(d) %in% y_aes])
        list(
          x = if (length(x) == 0) NULL else range(x),
          y = if (length(y) == 0) NULL else range(y)
        )
      }
    })
  },
  get_frame_vars = function(self, params) {
    NULL
  }
)
#' @importFrom ggplot2 ggplot_add
#' @export
ggplot_add.View <- function(object, plot, ...) {
  plot <- as.gganim(plot)
  plot$view <- object
  plot
}
