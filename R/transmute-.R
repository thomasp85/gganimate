#' Define how entering and exiting data behaves
#'
#' The purpose of `enter_*()` and `exit_*()` is to control what happens with
#' data that does not persist during a tween. In general the non-persistent data
#' is transformed to an *invisible* version that can be tweened to, e.g. by
#' setting the opacity to 0 or be moving the element off-screen. It is possible
#' to define your own transformations, or rely on some of the build in
#' *effects*.
#'
#' @param early Should the data appear in the beginning of the transition or in
#' the end
#' @param fade Should the elements fade in/out in addition to the effect
#' @param default A default transformation to use
#' @param ... Additional specific transformations either named by the geom
#' (e.g. `bar`, or by its position in the layer stack, e.g. `"2"`)
#'
#' @details
#' For layers that are tweened based on the raw data, only the specified
#' aesthetics are available to modify, while all possible variables are
#' available for late-tweening layers
#'
#' **appear**/**disappear** will simply make elements appear/disappear at either the
#' start or end of the transition.
#'
#' **fade** will simply set the alpha value to zero making the elements fade
#' in/out during the transition.
#'
#' **grow**/**shrink** will set the elements to zero size making them gradually
#' grow into / shrink out of existence. Zero size depends on the type of layer,
#' e.g. polygons/paths will have all their points set to the mean, while points
#' will have size/stroke set to zero.
#'
#' @name enter_exit
#' @rdname enter_exit
#' @aliases enter exit
#'
NULL

#' @importFrom ggplot2 ggproto
create_factory <- function(type, default, ...) {
  fac_class <- switch(type, enter = 'EnterFactory', exit = 'ExitFactory')
  factories <- prepare_factories(default, ...)
  ggproto(fac_class, TransmuteFactory,
    default = factories$default,
    positions = factories$positions,
    classes = factories$classes
  )
}

#' @importFrom ggplot2 ggproto
TransmuteFactory <- ggproto('TransmuteFactory', NULL,
  default = NULL,
  positions = list(),
  classes = list(),
  get_factories = function(self, layers) {
    if (length(self$positions) != 0) {
      factories <- rep(list(self$default), length(layers))
      factories[as.integer(names(self$positions))] <- self$positions
    } else if (length(self$classes) != 0) {
      lapply(layers, function(l) {
        best <- inherits(l$geom, names(self$classes), which = TRUE)
        if (all(best == 0)) {
          self$default
        } else {
          best <- which(best != 0 & best == min(best[best != 0]))
          self$classes[[best[1]]]
        }
      })
    } else {
      rep(list(self$default), length(layers))
    }
  }
)
#' @export
#' @importFrom ggplot2 ggplot_add
ggplot_add.EnterFactory <- function(object, plot, object_name) {
  plot <- as.gganim(plot)
  plot$transmuters$add_enter(object)
  plot
}
#' @export
#' @importFrom ggplot2 ggplot_add
ggplot_add.ExitFactory <- function(object, plot, object_name) {
  plot <- as.gganim(plot)
  plot$transmuters$add_exit(object)
  plot
}

# HELPERS -----------------------------------------------------------------

#' @importFrom rlang as_function
maybe_function <- function(x) if (is.null(x)) x else as_function(x)
capitalise <- function(x) {
  x1 <- substring(x, 1L, 1L)
  if (nchar(x) >= 3L && x1 %in% c("'", "\""))
    paste0(x1, toupper(substring(x, 2L, 2L)), tolower(substring(x, 3L)))
  else paste0(toupper(x1), tolower(substring(x, 2L)))
}
prepare_factories <- function(default, ...) {
  default <- maybe_function(default)
  specials <- lapply(list(...), maybe_function)
  positions <- list()
  classes <- list()
  if (length(specials) != 0) {
    if (all(grepl('^\\d+$', names(specials)))) {
      if (is.null(names(specials))) names(specials) <- as.character(seq_along(specials))
      positions = specials
    } else {
      geoms <- sub('geom_?', '', names(specials), ignore.case = TRUE)
      names(specials) <- paste0('Geom', vapply(tolower(geoms), capitalise, character(1)))
      classes <- specials
    }
  }
  list(default = default, positions = positions, classes = classes)
}
