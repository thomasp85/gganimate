.store <- new.env(parent = emptyenv())

#' Retrieve the last rendered animation
#'
#' @export
#' @keywords internal
last_animation <- function() .store$animation
#' @rdname last_animation
#' @export
set_last_animation <- function(value) .store$animation <- value
