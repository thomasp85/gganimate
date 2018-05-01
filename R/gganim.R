#' @importFrom ggplot2 is.ggplot
as.gganim <- function(x) {
  if (is.gganim(x)) return(x)
  if (!is.ggplot(x)) stop('Only knows how to convert ggplot to gganim', call. = FALSE)
  class(x) <- c('gganim', class(x))
  x$transition <- transition_null()
  x$view <- view_static()
  x$transmuters <- transmuter_list()
  x
}
is.gganim <- function(x) inherits(x, 'gganim')

#' @export
print.gganim <- function(x, ...) {
  cat('A gganim object\n')
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
