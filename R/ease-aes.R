#' Control easing of aesthetics
#'
#' Easing defines how a value change to another during tweening. Will it
#' progress linearly, or maybe start slowly and then build up momentum. In
#' `gganimate`, each aesthetic or computed variable can be tweened with
#' individual easing functions using the `ease_aes()` function. All easing
#' functions implemented in `tweenr` are available, see [tweenr::display_ease]
#' for an overview.
#'
#' @param default The default easing function to use
#' @param ... Override easing for specific aesthetics
#'
#' @export
#' @importFrom ggplot2 ggproto
#'
ease_aes <- function(default, ...) {
  aesthetics <- list(...)
  if (length(aesthetics) > 0) {
    if (is.null(names(aesthetics))) stop('Aesthetics must be named', call. = FALSE)
    if ('x' %in% names(aesthetics)) {
      aesthetics[x_aes] <- aesthetics['x']
    }
    if ('y' %in% names(aesthetics)) {
      aesthetics[y_aes] <- aesthetics['y']
    }
    aes_names <- sub('color', 'colour', names(aesthetics))
    aesthetics <- unlist(aesthetics)
  } else {
    aes_names <- character()
    aesthetics <- character()
  }
  ggproto(NULL, EaseAes, default = default, aesthetics = aesthetics, aes_names = aes_names)
}

EaseAes <- ggproto("EaseAes", NULL,
  default = NULL,
  aesthetics = character(),
  aes_names = character(),
  get_ease = function(self, layer_data) {
    lapply(layer_data, function(d) {
      base <- rep(self$default, ncol(d))
      common_aes <- intersect(names(d), self$aes_names)
      if (length(common_aes) > 0) {
        base[match(common_aes, names(d))] <- self$aesthetics[match(common_aes, self$aes_names)]
      }
      base
    })
  }
)

#' @importFrom ggplot2 ggplot_add
#' @export
ggplot_add.EaseAes <- function(object, plot, object_name) {
  plot <- as.gganim(plot)
  plot$ease <- object
  plot
}
