#' @rdname enter_exit
#' @export
exit_manual <- function(default = NULL, ..., name = 'manual') {
  create_factory('exit', default, ..., name = name)
}
#' @rdname enter_exit
#' @export
exit_disappear <- function(early = FALSE, ...) {
  transmute_appear('exit', early = !early, ..., name = 'disappear')
}
#' @rdname enter_exit
#' @export
exit_fade <- function(..., alpha = 0) {
  transmute_fade('exit', ..., alpha = alpha, name = 'fade')
}
#' @rdname enter_exit
#' @export
exit_shrink <- function(..., size = 0) {
  transmute_grow('exit', ..., size = size, name = 'shrink')
}
#' @rdname enter_exit
#' @export
exit_recolour <- function(..., colour = 'white', fill = colour) {
  transmute_recolour('exit', ..., colour = colour, fill = fill, name = 'recolour')
}
#' @rdname enter_exit
#' @export
exit_recolor <- function(..., color = 'white', fill = color) {
  exit_recolour(..., colour = color, fill = fill)
}
#' @rdname enter_exit
#' @export
exit_fly <- function(..., x_loc = NA, y_loc = NA) {
  transmute_fly('exit', ..., x_loc = x_loc, y_loc = y_loc, name = 'fly')
}
#' @rdname enter_exit
#' @export
exit_drift <- function(..., x_mod = 0, y_mod = 0) {
  transmute_drift('exit', ..., x_mod = x_mod, y_mod = y_mod, name = 'drift')
}


# RESET -------------------------------------------------------------------

#' @rdname enter_exit
#' @export
exit_reset <- function() {
  create_resetter('exit')
}
