#' Create a temporary file within the temporary directory
#'
#' This is necessary because the animation package often copies
#' to the temporary directory, which leads to animation trying to
#' copy a file to itself.
#'
#' @param pattern the initial part of the name
#' @param fileext file extension
gganimate_tempfile <- function(pattern = "file", fileext = "") {
  outdir <- file.path(tempdir(), "gganimate")
  dir.create(outdir, showWarnings = FALSE)
  tempfile(pattern, outdir, fileext = fileext)
}


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
  # browser()
  if (is.null(vp)) {
    grid::grid.draw(gtable)
  } else {
    if (is.character(vp)) grid::seekViewport(vp) else grid::pushViewport(vp)
    grid::grid.draw(gtable)
    grid::upViewport()
  }
}


#' Auto browse to a filename
#'
#' This utility function is adapted from the animation package
#' \url{https://github.com/yihui/animation/blob/df12e57b3cb1a71a1935f5351e007d141af8ae2c/R/utils.R}
#'
#' @param output Open a file
auto_browse = function(output){
  if (.Platform$OS.type == 'windows') {
    try(shell.exec(output))
  } else if (Sys.info()['sysname'] == 'Darwin') {
    system(paste('open ', shQuote(output)))
  } else system(paste('xdg-open ', shQuote(output)))
}
