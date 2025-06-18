#' @importFrom ggplot2 is_ggplot
as.gganim <- function(x) {
  if (is.gganim(x)) return(x)
  if (!is_ggplot(x)) cli::cli_abort('Only knows how to convert {.cls ggplot} objects to gganim')
  class(x) <- c('gganim', class(x))
  if (inherits(x, 'ggraph')) {
    cl <- class(x)
    ggraph_ind <- cl == 'ggraph'
    cl <- c(cl[ggraph_ind], cl[!ggraph_ind])
    class(x) <- cl
  }
  x$transition <- transition_null()
  x$view <- view_static()
  x$shadow <- shadow_null()
  x$transmuters <- transmuter_list()
  x$ease <- ease_aes('linear')
  x
}
is.gganim <- function(x) inherits(x, 'gganim')

#' @export
plot.gganim <- function(x, frame = 50, total = 100, detail = 1, newpage = is.null(vp), vp = NULL, ...) {
  plot <- prerender(x, total * detail)
  plot$scene$plot_frame(plot, frame * detail, newpage = newpage, vp = vp)
  invisible(x)
}
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
  if (knitr::is_latex_output()) {
    knitr_options$device <- 'current'
    inject(animate(plot = x, !!!knitr_options))
  } else {
    anim <- inject(animate(plot = x, !!!knitr_options))
    knitr::knit_print(anim, options, ...)
  }
}
get_knitr_options <- function(options, unlist = TRUE) {
  opt <- options$gganimate
  opt$device <- opt$device %||% options$dev
  if (is.null(opt$width) || is.null(opt$height)) {
    opt$width <- options$fig.width
    opt$height <- options$fig.height
    opt$units <- 'in'
    opt$res <- options$dpi
  }
  if (unlist) {
    c(opt, options$dev.args)
  } else {
    opt$dev_args <- options$dev.args %||% list()
    opt$dev_args <- modifyList(opt$dev_args, opt[c('width', 'height', 'units', 'res')])
    opt
  }
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
