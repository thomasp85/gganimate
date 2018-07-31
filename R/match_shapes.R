#' @rdname layer_type
#'
#' @export
match_shapes <- function(x) {
  UseMethod('match_shapes')
}
#' @export
match_shapes.default <- function(x) NULL
#' @export
match_shapes.StatContour <- function(x) FALSE
