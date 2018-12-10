#' Define what graphical type a layer is
#'
#' @param x a Layer, Geom, or Stat
#' @keywords internal
#' @export
layer_type <- function(x) UseMethod('layer_type')
#' @export
layer_type.default <- function(x) NULL
#' @export
layer_type.GeomPath <- function(x) 'path'
#' @export
layer_type.GeomRibbon <- function(x) 'path'
#' @export
layer_type.GeomSmooth <- function(x) 'path'
#' @export
layer_type.GeomPolygon <- function(x) 'polygon'
#' @export
layer_type.GeomSf <- function(x) 'sf'
#' @export
layer_type.Stat <- function(x) NULL
#' @export
layer_type.StatContour <- function(x) 'path'
