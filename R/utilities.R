#' Plot a built ggplot object
#'
#' We needed a customized version of ggplot2's \code{print.ggplot2},
#' because we need to build plots from the intermediate results of
#' \code{\link{ggplot_build}} rather than from a \code{gg} object.
#'
#' @param b A list resulting from \code{\link{ggplot_build}}
#' @param newpage draw new (empty) page first?
#' @param vp viewport to draw plot in
plot_ggplot_build <- function(b, newpage = is.null(vp), vp = NULL) {
  if (newpage) {
    grid::grid.newpage()
  }

  grDevices::recordGraphics(
    requireNamespace("ggplot2", quietly = TRUE),
    list(),
    getNamespace("ggplot2")
  )

  gtable <- ggplot_gtable(b)
  if (is.null(vp)) {
    grid::grid.draw(gtable)
  } else {
    if (is.character(vp)) grid::seekViewport(vp) else grid::pushViewport(vp)
    grid::grid.draw(gtable)
    grid::upViewport()
  }
}
