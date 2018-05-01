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
transmute_fade <- function(type, ...) {
  create_factory(type, default = function(x) {
    if (is.null(x$alpha)) {
      warning('Alpha not set on data', call. = FALSE)
    } else {
      x$alpha <- 0
    }
    x
  }, ...)
}
transmute_grow <- function(type, fade = FALSE, ...) {
  create_factory(
    type,
    default = function(x) {
      if (!is.null(x$size)) x$size <- 0
      if (!is.null(x$width)) x$width <- 0
      if (!is.null(x$stroke)) x$stroke <- 0
      if (fade && !is.null(x$alpha)) x$alpha <- 0
      x
    },
    polygon = function(x) {
      x$x <- mean(x$x)
      x$y <- mean(x$y)
      if (fade && !is.null(x$alpha)) x$alpha <- 0
    },
    path = function(x) {
      x$x <- mean(x$x)
      x$y <- mean(x$y)
      if (fade && !is.null(x$alpha)) x$alpha <- 0
    },
    boxplot = function(x) {
      x$ymin <- x$middle
      x$lower <- x$middle
      x$notchlower <- x$middle
      x$notchupper <- x$middle
      x$upper <- x$middle
      x$ymax <- x$middle
      if (fade && !is.null(x$alpha)) x$alpha <- 0
      x
    },
    bar = function(x) {
      x$y <- 0
      if (fade && !is.null(x$alpha)) x$alpha <- 0
      x
    },
    ...
  )
}
