#' @export
layer_type <- function(x) UseMethod('layer_type')
#' @export
layer_type.default <- function(x) 'point'
