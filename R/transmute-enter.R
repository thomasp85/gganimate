#' @rdname enter_exit
#' @export
enter_manual <- function(default = NULL, ..., name = 'manual') {
  create_factory('enter', default, ..., name = name)
}
#' @rdname enter_exit
#' @export
enter_appear <- function(early = FALSE, ...) {
  transmute_appear('enter', early = early, ..., name = 'appear')
}
#' @rdname enter_exit
#' @export
enter_fade <- function(..., alpha = 0) {
  transmute_fade('enter', ..., alpha = alpha, name = 'fade')
}
#' @rdname enter_exit
#' @export
enter_grow <- function(..., size = 0) {
  transmute_grow('enter', ..., size = size, name = 'grow')
}
#' @rdname enter_exit
#' @export
enter_recolour <- function(..., colour = 'white', fill = colour) {
  transmute_recolour('enter', ..., colour = colour, fill = fill, name = 'recolour')
}
#' @rdname enter_exit
#' @export
enter_recolor <- function(..., color = 'white', fill = color) {
  enter_recolour(..., colour = color, fill = fill)
}
#' @rdname enter_exit
#' @export
enter_fly <- function(..., x_loc = NA, y_loc = NA) {
  transmute_fly('enter', ..., x_loc = x_loc, y_loc = y_loc, name = 'fly')
}
#' @rdname enter_exit
#' @export
enter_drift <- function(..., x_mod = 0, y_mod = 0) {
  transmute_drift('enter', ..., x_mod = x_mod, y_mod = y_mod, name = 'drift')
}


# RESET -------------------------------------------------------------------

#' @rdname enter_exit
#' @export
enter_reset <- function() {
  create_resetter('enter')
}

