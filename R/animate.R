#' @importFrom grid grid.newpage grid.draw convertWidth convertHeight
#' @export
animate <- function(plot, nframes = NULL, fps = 10, length = NULL,
                    renderer = default_renderer, device = 'png', ...) {
  if (sum(c(is.null(nframes), is.null(fps), is.null(length))) > 1) {
    stop("At least 2 of 'nframes', 'fps', and 'length' must be given", call. = FALSE)
  }
  if (is.null(nframes)) {
    nframes <- round(length * fps)
  }
  if (is.null(fps)) {
    fps <- round(nframes / length)
  }
  plot <- set_nframes(plot, nframes)
  plot <- ggplot_build(plot)
  nframes_final <- get_nframes(plot)
  if (nframes != nframes_final) message('nframes adjusted to match plot')
  dir <- tempfile(pattern = '')
  dir.create(dir, showWarnings = FALSE)
  switch(
    device,
    png = png(file.path(dir, 'gganim_plot%04d.png'), ...),
    jpg =,
    jpeg = jpeg(file.path(dir, 'gganim_plot%04d.jpg'), ...)
  )
  frame <- plot$scene$get_frame(plot, 1)
  frame <- ggplot_gtable(frame)
  widths_rel <- frame$widths
  widths <- convertWidth(widths_rel, 'mm')
  null_widths <- as.numeric(widths) == 0
  widths[null_widths] <- widths_rel[null_widths]
  heights_rel <- frame$heights
  heights <- convertHeight(heights_rel, 'mm')
  null_heights <- as.numeric(heights) == 0
  heights[null_heights] <- heights_rel[null_heights]
  grid.draw(frame)
  for (i in seq_len(nframes_final)[-1]) {
    grid.newpage()
    frame <- plot$scene$get_frame(plot, i)
    frame <- ggplot_gtable(frame)
    frame$widths <- widths
    frame$heights <- heights
    grid.draw(frame)
  }
  dev.off()
  frames <- list.files(dir, 'gganim_plot', full.names = TRUE)
  renderer(frames, fps)
}
#' @importFrom magick image_read image_animate
#' @export
default_renderer <- function(frames, fps) {
  anim <- image_read(frames)
  image_animate(anim, fps)
}
