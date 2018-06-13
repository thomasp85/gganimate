.store <- new.env(parent = emptyenv())
set_last_animation <- function(value) .store$animation <- value

#' Retrieve the last rendered animation
#'
#' @export
#' @keywords internal
last_animation <- function() .store$animation
