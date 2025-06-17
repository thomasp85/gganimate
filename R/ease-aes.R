#' Control easing of aesthetics
#'
#' Easing defines how a value change to another during tweening. Will it
#' progress linearly, or maybe start slowly and then build up momentum. In
#' `gganimate`, each aesthetic or computed variable can be tweened with
#' individual easing functions using the `ease_aes()` function. All easing
#' functions implemented in `tweenr` are available, see [tweenr::display_ease]
#' for an overview. Setting an ease for `x` and/or `y` will also affect the
#' other related positional aesthetics (e.g. `xmin`, `yend`, etc).
#'
#' @param default The default easing function to use (defaults to `'linear'`)
#' @param ... Override easing for specific aesthetics
#'
#' @section Easing functions:
#' - **quadratic** Models a power-of-2 function
#' - **cubic** Models a power-of-3 function
#' - **quartic** Models a power-of-4 function
#' - **quintic** Models a power-of-5 function
#' - **sine** Models a sine function
#' - **circular** Models a pi/2 circle arc
#' - **exponential** Models an exponential function
#' - **elastic** Models an elastic release of energy
#' - **back** Models a pullback and relase
#' - **bounce** Models the bouncing of a ball
#'
#' *Modifiers* \cr
#' - **-in** The easing function is applied as-is
#' - **-out** The easing function is applied in reverse
#' - **-in-out** The first half of the transition it is applied as-is, while in
#'   the last half it is reversed
#'
#' @export
#' @importFrom ggplot2 ggproto
#'
#' @examples
#' anim <- ggplot(mtcars, aes(mpg, disp)) +
#'   transition_states(gear, transition_length = 2, state_length = 1) +
#'   enter_fade() +
#'   exit_fade()
#'
#' \dontrun{
#' # Default uses linear easing
#' animate(anim)
#' }
#'
#' # Change all to 'cubic-in-out' for a smoother appearance
#' anim1 <- anim +
#'   ease_aes('cubic-in-out')
#' \dontrun{
#' animate(anim1)
#' }
#'
#' # Only change easing of y variables
#' anim2 <- anim +
#'   ease_aes(y = 'bounce-in')
#' \dontrun{
#' animate(anim2)
#' }
#'
ease_aes <- function(default = 'linear', ...) {
  aesthetics <- list(...)
  if (length(aesthetics) > 0) {
    if (is.null(names(aesthetics))) cli::cli_abort('Aesthetics must be named')
    if ('x' %in% names(aesthetics)) {
      aesthetics[setdiff(x_aes, names(aesthetics))] <- aesthetics['x']
    }
    if ('y' %in% names(aesthetics)) {
      aesthetics[setdiff(y_aes, names(aesthetics))] <- aesthetics['y']
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
ggplot_add.EaseAes <- function(object, plot, ...) {
  plot <- as.gganim(plot)
  plot$ease <- object
  plot
}
