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
exit_fade <- function(...) {
  transmute_fade('exit', ...)
}
#' @rdname enter_exit
#' @export
exit_shrink <- function(fade = FALSE, ...) {
  transmute_grow('exit', fade = fade, ...)
}
