#' Define if layers should be tweened before stats are calculated
#'
#' @param x a Layer, Geom, or Stat
#' @keywords internal
#' @export
tween_before_stat <- function(x) UseMethod('tween_before_stat')
#' @export
tween_before_stat.default <- function(x) FALSE
