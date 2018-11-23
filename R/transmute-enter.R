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
enter_fade <- function(..., alpha = 0) {
  transmute_fade('enter', ..., alpha = alpha)
}
#' @rdname enter_exit
#' @export
enter_grow <- function(..., size = 0, alpha = NA) {
  transmute_grow('enter', ..., size = size, alpha = alpha)
}
