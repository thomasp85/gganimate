#' @rdname enter_exit
#' @export
enter_manual <- function(default, ...) {
  create_factory('enter', default, ...)
}
#' @rdname enter_exit
#' @export
enter_appear <- function(early = FALSE, ...) {
  transmute_appear('enter', early = early, ...)
}
#' @rdname enter_exit
#' @export
enter_fade <- function(...) {
  transmute_fade('enter', ...)
}
#' @rdname enter_exit
#' @export
enter_grow <- function(fade = FALSE, ...) {
  transmute_grow('enter', fade = fade, ...)
}
