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
#' @param alpha The start/end transparency.
#' @param colour,color,fill The start/end colour and fill the elements should
#' (dis)appear into
#' @param x_loc,y_loc Start and end positions of the graphic elements
#' @param x_mod,y_mod Modification to add to the entering or exiting data
#' @param size The proportional start/end size. `0` means complete shrinking
#' while `1` means no shrinking
#' @param default A default transformation to use
#' @param name A name for the manual modification (only used when printing the
#' object)
#' @param ... Additional specific transformations either named by the geom
#' (e.g. `bar`, or by its position in the layer stack, e.g. `"2"`)
#'
#' @section User-defined transformations:
#' All enter/exit functions allows the user to add additional transformation
#' functions targeting specific layers. If the functions are named, then the
#' name is understood to reference the class of geoms it applies to. If the
#' functions are unnamed or numbered they will apply to the layer with a
#' matching index in the stack. Named and indexed transformations cannot be
#' mixed.
#'
#' All modifications except `enter_manual()`/`exit_manual()` sets a range of
#' modifications already, but further can be added with the `...`. For the
#' manual versions a `default` transformation can be set which will apply to all
#' layers that does not match any of the other given transformations. Often a
#' single default transformation is enough and no specific transformations are
#' needed.
#'
#' Transformation can be given as any expression that can be converted with
#' [rlang::as_function()]. This means that `purrr` style lambda functions are
#' allowed in addition to anonymous functions etc. Transformation functions must
#' accept a data.frame and return a data.frame of the same dimensions. The
#' function will be called with the entering/exiting layer data, except for the
#' case of polygon- and path-like layers in which case the function recieves the
#' entering/exiting polygon/path data one by one. A special option is to set a
#' transformation as `NULL` instead of a function. In that case the entering and
#' exiting data will simply appear/disappear when it is no longer part of a
#' frame.
#'
#' @section Modification composition:
#' Enter and exit modifications are composable so that multiple different ones
#' can be added to an animation and will be applied in turn. You can also
#' combine multiples and save them as a new enter or exit modification using
#' `c()`.
#'
#' Due to the composable nature of enter and exit modifications it is not
#' possible to overwrite a prior modification by adding a new. If it is needed
#' to start from scratch then the sentinels `enter_reset()` and `exit_reset()`
#' are provided which clears all prior modifications.
#'
#' @section Modification types:
#' A range of modification types are provided by `gganimate` and using
#' `enter_manual()`/`exit_manual()` or modification composition it is possible
#' to create your own.
#'
#' **appear**/**disappear** will simply make elements appear/disappear at either
#' the start or end of the transition. The default if nothing else is added.
#'
#' **fade** will simply set the alpha value to zero making the elements fade
#' in/out during the transition.
#'
#' **grow**/**shrink** will set the elements to zero size making them gradually
#' grow into / shrink out of existence. Zero size depends on the type of layer,
#' e.g. polygons/paths will have all their points set to the mean, while points
#' will have size/stroke set to zero.
#'
#' **recolour**/**recolor** will change the colour and/or fill of the elements
#' making them gradually change from the defined colour and into their try
#' colour. Be aware that unless the colour and fill are set to the same as the
#' background colour of the plot this modification needs to be combined with
#' others to ensure that elements does not abruptly appear.
#'
#' **fly** will set a specific x and y position where all elements will enter
#' from/ exit to, irrespectible of their real position.
#'
#' **drift** will modify the real position of the entering and exiting elements
#' by a specified amount, e.g. setting `x_mod = -5` will let all elements enter
#' from/exit to the left with a terminal position 5 points to the left of the
#' real position.
#'
#' @name enter_exit
#' @rdname enter_exit
#' @aliases enter exit
#'
#' @examples
#' # Default is appear/disappear
#' anim <- ggplot(mtcars, aes(factor(gear), mpg)) +
#'   geom_boxplot() +
#'   transition_states(gear, 2, 1)
#'
#' # Fade-in, fly-out
#' anim1 <- anim +
#'   enter_fade() +
#'   exit_fly(x_loc = 7, y_loc = 40)
#'
#' # Enter and exit accumulates
#' anim2 <- anim +
#'   enter_fade() + enter_grow() +
#'   exit_fly(x_loc = 7, y_loc = 40) + exit_recolour(fill = 'forestgreen')
#'
NULL

#' @importFrom ggplot2 ggproto
create_factory <- function(type, default, ..., name = 'manual') {
  fac_class <- switch(type, enter = 'EnterFactory', exit = 'ExitFactory')
  factories <- prepare_factories(default, ...)
  ggproto(fac_class, TransmuteFactory,
    factory_layer = list(list(
      default = factories$default,
      positions = factories$positions,
      classes = factories$classes
    )),
    name = name
  )
}
create_resetter <- function(type) {
  fac_class <- switch(type, enter = 'EnterFactory', exit = 'ExitFactory')
  ggproto(fac_class, TransmuteFactory, reset = TRUE)
}
#' @importFrom ggplot2 ggproto
TransmuteFactory <- ggproto('TransmuteFactory', NULL,
  factory_layer = list(),
  name = '',
  reset = FALSE,
  print = function(self, ...) {
    cat('<', class(self)[1], if (self$reset) ' (Resetter)' else '', '>\n', sep = '')
    if (!self$reset) cat(' Stages: ', paste(self$name, collapse = ', '), '\n', sep = '')
    invisible(self)
  },
  add_factory = function(self, x) {
    if (!self$inherit(x)) cli::cli_abort("Can only combine factories of the same class")
    if (x$reset) {
      self$factory_layer <- list()
      self$name <- character()
      self$reset <- TRUE
    } else {
      self$factory_layer <- c(self$factory_layer, x$factory_layer)
      self$name <- c(self$name, x$name)
      self$reset <- FALSE
    }
    invisible(self)
  },
  inherit = function(self, x) {
    inherits(x, class(self)[1])
  },
  get_factories = function(self, layers) {
    f_layers <- lapply(self$factory_layer, function(f_layer) {
      if (length(f_layer$positions) != 0) {
        factories <- rep(list(f_layer$default), length(layers))
        factories[as.integer(names(f_layer$positions))] <- f_layer$positions
      } else if (length(f_layer$classes) != 0) {
        lapply(layers, function(l) {
          best <- inherits(l$geom, names(f_layer$classes), which = TRUE)
          if (all(best == 0)) {
            f_layer$default
          } else {
            best <- which(best != 0 & best == min(best[best != 0]))
            f_layer$classes[[best[1]]]
          }
        })
      } else {
        rep(list(f_layer$default), length(layers))
      }
    })
    lapply(seq_along(layers), function(i) {
      factory_line <- lapply(f_layers, `[[`, i)
      if (any(vapply(factory_line, is.null, logical(1)))) return(NULL)
      function(x) {
        for (i in seq_along(factory_line)) {
          x <- factory_line[[i]](x)
        }
        x
      }
    })
  },
  clone = function(self) {
    ggproto(NULL, self)
  }
)
#' @export
c.TransmuteFactory <- function(...) {
  factories <- list(...)
  if (length(factories) == 1) return(factories[[1]])
  # This is solemnly there to please the byte-compiler from R3.6 and onwards
  # "Everything under heaven is in utter chaos; the situation is excellent."
  if (!all(vapply(factories, inherits, logical(1), what = 'TransmuteFactory'))) return(factories)
  factories[[1]] <- factories[[1]]$clone()
  Reduce(function(l, r) l$add_factory(r), factories)
}
#' @export
#' @importFrom ggplot2 ggplot_add
ggplot_add.EnterFactory <- function(object, plot, ...) {
  plot <- as.gganim(plot)
  plot$transmuters <- plot$transmuters$clone()
  plot$transmuters$add_enter(object)
  plot
}
#' @export
#' @importFrom ggplot2 ggplot_add
ggplot_add.ExitFactory <- function(object, plot, ...) {
  plot <- as.gganim(plot)
  plot$transmuters <- plot$transmuters$clone()
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
