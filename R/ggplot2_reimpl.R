plot_clone <- function(plot) {
  p <- plot
  p$scales <- plot$scales$clone()
  p$coordinates <- ggproto(NULL, plot$coordinates)
  p$transmuters <- plot$transmuters$clone()
  p
}

is.waive <- function (x) inherits(x, "waiver")

data_frame0 <- function(...) vctrs::data_frame(..., .name_repair = "minimal")

unique0 <- function(x, ...) if (is.null(x)) x else vctrs::vec_unique(x, ...)

# Reimplement get_scales in Layout to allow panels with frame info to get the
# correct scales
#' @importFrom ggplot2 FacetNull CoordCartesian Layout ggproto
create_layout <- function(facet = FacetNull, coord = CoordCartesian) {
  ggproto(NULL, Layout,
    facet = facet,
    coord = coord,
    get_scales = function(self, i) {
      if (is.character(i)) i <- as.integer(strsplit(i, '<')[[1]][1])
      this_panel <- self$layout[self$layout$PANEL == i, ]

      list(
        x = self$panel_scales_x[[this_panel$SCALE_X]],
        y = self$panel_scales_y[[this_panel$SCALE_Y]]
      )
    }
  )
}

deprecate_soft0 <- function(..., user_env = NULL) {
  user_env <- user_env %||% getOption("ggplot2_plot_env") %||% caller_env(2)
  lifecycle::deprecate_soft(..., user_env = user_env)
}

# Restart handler for using vec_rbind with mix of types
# Ordered is coerced to factor
# If a character vector is present the other is converted to character
with_ordered_restart <- function(expr, .call) {
  withCallingHandlers(
    expr,
    vctrs_error_incompatible_type = function(cnd) {
      x <- cnd[["x"]]
      y <- cnd[["y"]]

      class_x <- class(x)[1]
      class_y <- class(y)[1]

      restart <- FALSE

      if (is.ordered(x) || is.ordered(y)) {
        restart <- TRUE
        if (is.ordered(x)) {
          x <- factor(as.character(x), levels = levels(x))
        }
        if (is.ordered(y)) {
          y <- factor(as.character(y), levels = levels(y))
        }
      } else if (is.character(x) || is.character(y)) {
        restart <- TRUE
        if (is.character(x)) {
          y <- as.character(y)
        } else {
          x <- as.character(x)
        }
      } else if (is.factor(x) || is.factor(y)) {
        restart <- TRUE
        lev <- c()
        if (is.factor(x)) {
          lev <- c(lev, levels(x))
        }
        if (is.factor(y)) {
          lev <- c(lev, levels(y))
        }
        x <- factor(as.character(x), levels = unique(lev))
        y <- factor(as.character(y), levels = unique(lev))
      }

      # Don't recurse and let ptype2 error keep its course
      if (!restart) {
        return(zap())
      }

      msg <- paste0("Combining variables of class <", class_x, "> and <", class_y, ">")
      desc <- paste0(
        "Please ensure your variables are compatible before plotting (location: ",
        format_error_call(.call),
        ")"
      )

      deprecate_soft0(
        "3.4.0",
        I(msg),
        details = desc
      )

      x_arg <- cnd[["x_arg"]]
      y_arg <- cnd[["y_arg"]]
      call <- cnd[["call"]]

      # Recurse with factor methods and restart with the result
      if (inherits(cnd, "vctrs_error_ptype2")) {
        out <- vctrs::vec_ptype2(x, y, x_arg = x_arg, y_arg = y_arg, call = call)
        restart <- "vctrs_restart_ptype2"
      } else if (inherits(cnd, "vctrs_error_cast")) {
        out <- vctrs::vec_cast(x, y, x_arg = x_arg, to_arg = y_arg, call = call)
        restart <- "vctrs_restart_cast"
      } else {
        return(zap())
      }

      # Old-R compat for `tryInvokeRestart()`
      try_restart <- function(restart, ...) {
        if (!is_null(findRestart(restart))) {
          invokeRestart(restart, ...)
        }
      }
      try_restart(restart, out)
    }
  )
}

vec_rbind0 <- function(..., .error_call = current_env(), .call = caller_env()) {
  with_ordered_restart(
    vctrs::vec_rbind(..., .error_call = .error_call),
    .call
  )
}
