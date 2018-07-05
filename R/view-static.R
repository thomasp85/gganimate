#' Keep a fixed view that include all of the data
#'
#' This view keeps positional scales fixed across all frames
#'
#' @family views
#'
#' @export
#' @importFrom ggplot2 ggproto
view_static <- function() {
  ggproto("ViewStatic", View)
}
