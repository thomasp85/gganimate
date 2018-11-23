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
transmute_fade <- function(type, ..., alpha = 0) {
  create_factory(type, default = fade_elements(alpha), ...)
}
transmute_grow <- function(type, ..., size = 0, alpha = NA) {
  fade <- !is.na(alpha)
  fader <- if (fade) fade_elements(alpha)
  create_factory(
    type,
    default = function(x) {
      if (!is.null(x$size)) x$size <- size * x$size
      if (!is.null(x$width)) x$width <- size * x$width
      if (!is.null(x$stroke)) x$stroke <- size * x$stroke
      if (!is.null(x$edge_size)) x$edge_size <- size * x$edge_size
      if (!is.null(x$edge_width)) x$edge_width <- size * x$edge_width
      if (fade) x <- fader(x)
      x
    },
    polygon = function(x) {
      mean_x <- vapply(split(x$x, x$group), mean, numeric(1))
      mean_y <- vapply(split(x$y, x$group), mean, numeric(1))
      group <- match(x$group, names(mean_x))
      if (size == 0) {
        x$x <- mean_x[group]
        x$y <- mean_y[group]
      } else {
        x$x <- tween_at(mean_x[group], x$x, size, 'linear')
        x$y <- tween_at(mean_y[group], x$y, size, 'linear')
      }
      if (fade) x <- fader(x)
      x
    },
    violin = function(x) {
      mean_x <- vapply(split(x$x, x$group), mean, numeric(1))
      mean_y <- vapply(split(x$y, x$group), mean, numeric(1))
      group <- match(x$group, names(mean_x))
      if (size == 0) {
        x$x <- mean_x[group]
        x$y <- mean_y[group]
      } else {
        x$x <- tween_at(mean_x[group], x$x, size, 'linear')
        x$y <- tween_at(mean_y[group], x$y, size, 'linear')
      }
      if (fade) x <- fader(x)
      x
    },
    path = function(x) {
      mean_x <- vapply(split(x$x, x$group), mean, numeric(1))
      mean_y <- vapply(split(x$y, x$group), mean, numeric(1))
      group <- match(x$group, names(mean_x))
      if (size == 0) {
        x$x <- mean_x[group]
        x$y <- mean_y[group]
      } else {
        x$x <- tween_at(mean_x[group], x$x, size, 'linear')
        x$y <- tween_at(mean_y[group], x$y, size, 'linear')
      }
      if (fade) x <- fader(x)
      x
    },
    boxplot = function(x) {
      if (size == 0) {
        x$ymin <- x$middle
        x$lower <- x$middle
        x$notchlower <- x$middle
        x$notchupper <- x$middle
        x$upper <- x$middle
        x$ymax <- x$middle
      } else {
        x$ymin <- tween_at(x$middle, x$ymin, size, 'linear')
        x$lower <- tween_at(x$middle, x$lower, size, 'linear')
        x$notchlower <- tween_at(x$middle, x$notchlower, size, 'linear')
        x$notchupper <- tween_at(x$middle, x$notchupper, size, 'linear')
        x$upper <- tween_at(x$middle, x$upper, size, 'linear')
        x$ymax <- tween_at(x$middle, x$ymax, size, 'linear')
      }
      if (fade) x <- fader(x)
      x
    },
    bar = function(x) {
      x$y <- x$y * size
      if (fade) x <- fader(x)
      x
    },
    ...
  )
}

fade_elements <- function(alpha = 0) {
  function(x) {
    if (!is.null(x$edge_alpha)) {
      no_alpha <- is.na(x$edge_alpha)
      x$edge_alpha[!no_alpha] <- alpha
    } else if (!is.null(x$alpha)) {
      no_alpha <- is.na(x$alpha)
      x$alpha[!no_alpha] <- alpha
    } else {
      no_alpha <- TRUE
    }
    if (!is.null(x$colour)) x$colour[no_alpha] <- alpha(x$colour[no_alpha], alpha)
    if (!is.null(x$fill)) x$fill[no_alpha] <- alpha(x$fill[no_alpha], alpha)
    if (!is.null(x$edge_colour)) x$edge_colour[no_alpha] <- alpha(x$edge_colour[no_alpha], alpha)
    if (!is.null(x$edge_fill)) x$edge_fill[no_alpha] <- alpha(x$edge_fill[no_alpha], alpha)
    x
  }
}
