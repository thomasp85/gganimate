transmuter_list <- function() {
  ggproto(NULL, TransmuterList)
}
TransmuterList <- ggproto('TransmuterList', NULL,
  enter = NULL,
  exit = NULL,
  enter_factories = list(),
  exit_factories = list(),
  add_enter = function(self, enter) {
    if (enter$reset) {
      self$enter <- NULL
    } else if (is.null(self$enter)) {
      self$enter <- enter
    } else {
      self$enter$add_factory(enter)
    }
  },
  add_exit = function(self, exit) {
    if (exit$reset) {
      self$exit <- NULL
    } else if (is.null(self$exit)) {
      self$exit <- exit
    } else {
      self$exit$add_factory(exit)
    }
  },
  setup = function(self, layers) {
    if (is.null(self$enter)) self$enter <- enter_appear()
    self$enter_factories <- self$enter$get_factories(layers)
    if (is.null(self$exit)) self$exit <- exit_disappear()
    self$exit_factories <- self$exit$get_factories(layers)
  },
  enter_transmuters = function(self, i = NULL) {
    if (is.null(i)) i <- seq_along(self$enter_factories)
    self$enter_factories[i]
  },
  exit_transmuters = function(self, i = NULL) {
    if (is.null(i)) i <- seq_along(self$exit_factories)
    self$exit_factories[i]
  },
  clone = function(self) {
    ggproto(NULL, self,
      enter = if (is.null(self$enter)) NULL else self$enter$clone(),
      exit = if (is.null(self$exit)) NULL else self$exit$clone()
    )
  }
)

transmute_appear <- function(type, ..., early = FALSE, name) {
  f <- if (early) {
    function(x) x
  } else {
    NULL
  }
  create_factory(type, default = f, ..., name = name)
}
#' @importFrom scales alpha
transmute_fade <- function(type, ..., alpha = 0, name) {
  create_factory(
    type,
    default = function(x) {
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
    },
    smooth = function(x) {
      if (!is.null(x$alpha)) {
        no_alpha <- is.na(x$alpha)
        x$alpha[!no_alpha] <- alpha
      }
      if (!is.null(x$fill)) x$fill[no_alpha] <- alpha(x$fill[no_alpha], alpha)
      if (!is.null(x$colour)) x$colour <- alpha(x$colour, alpha)
      x
    },
    ...,
    name = name
  )
}
transmute_grow <- function(type, ..., size = 0, name) {
  create_factory(
    type,
    default = function(x) {
      if (!is.null(x$size)) x$size <- size * x$size
      if (!is.null(x$width)) x$width <- size * x$width
      if (!is.null(x$stroke)) x$stroke <- size * x$stroke
      if (!is.null(x$edge_size)) x$edge_size <- size * x$edge_size
      if (!is.null(x$edge_width)) x$edge_width <- size * x$edge_width
      x
    },
    polygon = shrink_multielement(size),
    violin = shrink_multielement(size),
    path = shrink_multielement(size),
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
      x
    },
    bar = function(x) {
      x$y <- x$y * size
      x$ymax <- x$ymax * size
      x
    },
    col = function(x) {
      x$y <- x$y * size
      x$ymax <- x$ymax * size
      x
    },
    ...,
    name = name
  )
}
transmute_recolour <- function(type, ..., colour = 'white', fill = colour, name) {
  create_factory(
    type,
    default = function(x) {
      if (!is.na(colour) && !is.null(x$colour)) x$colour <- colour
      if (!is.na(fill) && !is.null(x$fill)) x$fill <- fill
      if (!is.na(colour) && !is.null(x$edge_colour)) x$edge_colour <- colour
      if (!is.na(fill) && !is.null(x$edge_fill)) x$edge_fill <- fill
      x
    },
    ...,
    name = name
  )
}
transmute_fly <- function(type, ..., x_loc = NA, y_loc = NA, name) {
  if (is.na(x_loc) && is.na(y_loc)) cli::cli_warn('Both {.arg x_loc} and {.arg y_loc} are NA. No position change will occur.')
  create_factory(
    type,
    default = function(x) {
      if (!is.na(x_loc)) {
        included_x <- intersect(x_aes, names(x))
        x[, included_x] <- x[, included_x] - x[, 'x'] + x_loc
      }
      if (!is.na(y_loc)) {
        included_y <- intersect(y_aes, names(x))
        x[, included_y] <- x[, included_y] - x[, 'y'] + y_loc
      }
      x
    },
    boxplot = function(x) {
      if (!is.na(x_loc)) {
        included_x <- intersect(x_aes, names(x))
        x[, included_x] <- x[, included_x] - x[, 'x'] + x_loc
      }
      if (!is.na(y_loc)) {
        included_y <- intersect(y_aes, names(x))
        x[, included_y] <- x[, included_y] - (x[, 'lower'] + x[, 'upper'])/2 + y_loc
      }
      x
    },
    polygon = move_multielement(x_loc, y_loc),
    violin = move_multielement(x_loc, y_loc),
    path = move_multielement(x_loc, y_loc),
    ...,
    name = name
  )
}
transmute_drift <- function(type, ..., x_mod = 0, y_mod = 0, name) {
  if (x_mod == 0 && y_mod == 0) cli::cli_warn('Both {.arg x_mod} and {.arg y_mod} are 0. No position change will occur.')
  create_factory(
    type,
    default = function(x) {
      included_x <- intersect(x_aes, names(x))
      x[, included_x] <- x[, included_x] + x_mod
      included_y <- intersect(y_aes, names(x))
      x[, included_y] <- x[, included_y] + y_mod
      x
    },
    ...,
    name = name
  )
}


# HELPERS -----------------------------------------------------------------


shrink_multielement <- function(size = 0) {
  function(x) {
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
    x
  }
}
move_multielement <- function(x_loc, y_loc) {
  function(x) {
    if (!is.na(x_loc)) {
      mean_x <- vapply(split(x$x, x$group), mean, numeric(1))
      group <- match(x$group, names(mean_x))
      x$x <- x$x - mean_x[group] + x_loc
    }
    if (!is.na(y_loc)) {
      mean_y <- vapply(split(x$y, x$group), mean, numeric(1))
      if (is.na(x_loc)) group <- match(x$group, names(mean_y))
      x$y <- x$y - mean_y[group] + y_loc
    }
    x
  }
}
