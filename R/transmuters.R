transmuter_list <- function() {
  ggproto(NULL, TransmuterList)
}
TransmuterList <- ggproto('TransmuterList', NULL,
  enter = NULL,
  exit = NULL,
  enter_factories = list(),
  exit_factories = list(),
  add_enter = function(self, enter) {
    self$enter <- enter
  },
  add_exit = function(self, exit) {
    self$exit <- exit
  },
  setup = function(self, layers) {
    if (is.null(self$enter)) self$enter <- enter_appear()
    self$enter_factories <- self$enter$get_factories(layers)
    if (is.null(self$exit)) self$exit <- exit_disappear()
    self$exit_factories <- self$exit$get_factories(layers)
  },
  clone = function(self) {
    ggproto(NULL, self, enter = self$enter, exit = self$exit)
  },
  enter_transmuters = function(self, i = NULL) {
    if (is.null(i)) i <- seq_along(self$enter_factories)
    self$enter_factories[i]
  },
  exit_transmuters = function(self, i = NULL) {
    if (is.null(i)) i <- seq_along(self$exit_factories)
    self$exit_factories[i]
  }
)

transmute_appear <- function(type, early = FALSE, ...) {
  f <- if (early) {
    function(x) x
  } else {
    NULL
  }
  create_factory(type, default = f, ...)
}
#' @importFrom scales alpha
transmute_fade <- function(type, ...) {
  create_factory(type, default = fade_elements, ...)
}
transmute_grow <- function(type, fade = FALSE, ...) {
  create_factory(
    type,
    default = function(x) {
      if (!is.null(x$size)) x$size <- 0
      if (!is.null(x$width)) x$width <- 0
      if (!is.null(x$stroke)) x$stroke <- 0
      if (!is.null(x$edge_size)) x$edge_size <- 0
      if (!is.null(x$edge_width)) x$edge_width <- 0
      if (fade) x <- fade_elements(x)
      x
    },
    polygon = function(x) {
      mean_x <- vapply(split(x$x, x$group), mean, numeric(1))
      mean_y <- vapply(split(x$y, x$group), mean, numeric(1))
      group <- match(x$group, names(mean_x))
      x$x <- mean_x[group]
      x$y <- mean_y[group]
      if (fade) x <- fade_elements(x)
      x
    },
    violin = function(x) {
      mean_x <- vapply(split(x$x, x$group), mean, numeric(1))
      mean_y <- vapply(split(x$y, x$group), mean, numeric(1))
      group <- match(x$group, names(mean_x))
      x$x <- mean_x[group]
      x$y <- mean_y[group]
      if (fade) x <- fade_elements(x)
      x
    },
    path = function(x) {
      mean_x <- vapply(split(x$x, x$group), mean, numeric(1))
      mean_y <- vapply(split(x$y, x$group), mean, numeric(1))
      group <- match(x$group, names(mean_x))
      x$x <- mean_x[group]
      x$y <- mean_y[group]
      if (fade) x <- fade_elements(x)
      x
    },
    boxplot = function(x) {
      x$ymin <- x$middle
      x$lower <- x$middle
      x$notchlower <- x$middle
      x$notchupper <- x$middle
      x$upper <- x$middle
      x$ymax <- x$middle
      if (fade) x <- fade_elements(x)
      x
    },
    bar = function(x) {
      x$y <- 0
      if (fade) x <- fade_elements(x)
      x
    },
    ...
  )
}

fade_elements <- function(x) {
  if (!is.null(x$edge_alpha)) {
    no_alpha <- is.na(x$edge_alpha)
    x$edge_alpha[!no_alpha] <- 0
  } else if (!is.null(x$alpha)) {
    no_alpha <- is.na(x$alpha)
    x$alpha[!no_alpha] <- 0
  } else {
    no_alpha <- TRUE
  }
  if (!is.null(x$colour)) x$colour[no_alpha] <- alpha(x$colour[no_alpha], 0)
  if (!is.null(x$fill)) x$fill[no_alpha] <- alpha(x$fill[no_alpha], 0)
  if (!is.null(x$edge_colour)) x$edge_colour[no_alpha] <- alpha(x$edge_colour[no_alpha], 0)
  if (!is.null(x$edge_fill)) x$edge_fill[no_alpha] <- alpha(x$edge_fill[no_alpha], 0)
  x
}
