#' @importFrom ggplot2 is.ggplot
as.gganim <- function(x) {
  if (is.gganim(x)) return(x)
  if (!is.ggplot(x)) stop('Only knows how to convert ggplot to gganim', call. = FALSE)
  class(x) <- c('gganim', class(x))
  x$transition <- transition_null()
  x$view <- view_static()
  x$shadow <- shadow_null()
  x$transmuters <- transmuter_list()
  x$ease <- ease_aes('linear')
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
#' @param options chunk options for the currently executing chunk
#' @export
knit_print.gganim <- function(x, options, ...) {
  knitr_options <- get_knitr_options(options)
  anim <- do.call(animate, c(list(plot = x), knitr_options))
  knitr::knit_print(anim, options, ...)
}
get_knitr_options <- function(options) {
  opt <- options$gganimate
  opt$fps <- opt$fps %||% if (!is.null(options$interval)) 1/options$interval else NULL
  opt$device <- opt$device %||% options$dev
  if (is.null(opt$width) || is.null(opt$height)) {
    opt$width <- options$fig.width
    opt$height <- options$fig.height
    opt$units <- 'in'
    opt$res <- options$dpi
  }
  c(opt, options$dev.args)
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
