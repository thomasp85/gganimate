#' @rdname enter_exit
#' @export
exit_manual <- function(default, ...) {
  create_factory('exit', default, ...)
}
#' @rdname enter_exit
#' @export
exit_disappear <- function(early = FALSE, ...) {
  transmute_appear('exit', early = !early, ...)
}
#' @rdname enter_exit
#' @export
exit_fade <- function(..., alpha = 0) {
  transmute_fade('exit', ..., alpha = alpha)
}
#' @rdname enter_exit
#' @export
exit_shrink <- function(..., size = 0, alpha = NA) {
  transmute_grow('exit', ..., size = size, alpha = alpha)
}
