#' Define what graphical type a layer is
#'
#' @param x a Layer, Geom, or Stat
#' @keywords internal
#' @export
layer_type <- function(x) UseMethod('layer_type')
#' @export
layer_type.default <- function(x) 'point'
#' @export
#layer_type.GeomPath <- function(x) 'path'
#' @export
#layer_type.Stat <- function(x) NULL
