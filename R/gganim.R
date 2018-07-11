#' @importFrom ggplot2 is.ggplot
as.gganim <- function(x) {
  if (is.gganim(x)) return(x)
  if (!is.ggplot(x)) stop('Only knows how to convert ggplot to gganim', call. = FALSE)
  class(x) <- c('gganim', class(x))
  x$transition <- transition_null()
  x$view <- view_static()
  x$shadow <- shadow_null()
  x$transmuters <- transmuter_list()
  x$ease <- ease_aes('cubic-in-out')
  x
}
is.gganim <- function(x) inherits(x, 'gganim')

#' @rdname animate
#' @export
print.gganim <- function(x, ...) {
  anim <- animate(x, ...)
  print(anim, info = FALSE)
}
#' @rdname animate
#' @importFrom ggplot2 ggplot ggplot_build ggplot_gtable
#' @importFrom patchwork wrap_ggplot_grob wrap_plots
plot.gganim <- function(x, nframes = 9, detail = 10, ...) {
  nframes_total <- (nframes - 1) * detail + 1
  x <- set_nframes(x, nframes_total)
  x <- ggplot_build(x)
  nframes_final <- get_nframes(x)
  frame_ind <- unique(round(seq(1, nframes_final, length.out = nframes)))
  tables <- lapply(frame_ind, function(i) {
    gt <- ggplot_gtable(x$scene$get_frame(x, i))
    wrap_ggplot_grob(gt)
  })
  wrap_plots(c(list(ggplot(), tables)))
}

# HELPERS -----------------------------------------------------------------

set_nframes <- function(plot, n) {
  plot$nframes <- n
  plot
}
get_nframes <- function(plot) {
  if (is.null(plot$scene)) plot$nframes
  else plot$scene$nframes
}
