#' Render a gganim object
#'
#' This function takes a gganim object and renders it into an animation. The
#' nature of the animation is dependent on the renderer, but defaults to using
#' `magick` to render it to a gif. The length and framerate is decided on render
#' time and can be any two combination of `nframes`, `fps`, and `length`.
#' Rendering is happening in discrete time units. This means that any event in
#' the animation is rounded of to the nearest frame (e.g. entering will always
#' take a whole number of frames). This means that rounding artefacts are
#' possible when only rendering few frames. To avoid this you can increase the
#' `detail` argument. `detail` will get multiplied to `nframes` and the
#' resulting number of frames will get calculated, but only `nframes` evenly
#' spaced frames are rendered.
#'
#' @param plot A `gganim` object
#' @param nframes The number of frames to render
#' @param fps The framerate of the animation in frames/sec
#' @param length The length of the animation in seconds
#' @param detail The number of additional frames to calculate, per frame
#' @param renderer The function used to render the generated frames into an
#' animation. Gets a vector of paths to images along with the framerate.
#' @param device The device to use for rendering the single frames. Possible
#' values are `'png'`, `'jpeg'`, `'tiff'`, and `'bmp'`. Defaults to `'png'`.
#' @param ref_frame The frame to use for fixing dimensions of the plot, e.g. the
#' space available for axis text. Defaults to the first frame. Negative values
#' counts backwards (-1 is the last frame)
#' @param ... Arguments passed on to the device
#'
#' @return The return value of the `renderer` function
#'
#' @note `print.gganim`()  is an alias for `animate()` in the same way as
#' `print.ggplot()` is an alias for `plot.ggplot()`
#'
#' @importFrom grid grid.newpage grid.draw convertWidth convertHeight
#' @importFrom grDevices png jpeg tiff bmp
#' @importFrom progress progress_bar
#' @export
animate <- function(plot, nframes = NULL, fps = 10, length = NULL, detail = 1,
                    renderer = default_renderer, device = 'png', ref_frame = 1,
                    ...) {
  if (sum(c(is.null(nframes), is.null(fps), is.null(length))) > 1) {
    stop("At least 2 of 'nframes', 'fps', and 'length' must be given", call. = FALSE)
  }
  nframes <- nframes %||% round(length * fps)
  fps <- fps %||% round(nframes / length)
  nframes_total <- (nframes - 1) * detail + 1
  plot <- set_nframes(plot, nframes_total)
  plot <- ggplot_build(plot)
  nframes_final <- get_nframes(plot)
  frame_ind <- unique(round(seq(1, nframes_final, length.out = nframes)))
  if (nframes != length(frame_ind)) message('nframes adjusted to match plot')
  dir <- tempfile(pattern = '')
  dir.create(dir, showWarnings = FALSE)
  switch(
    device,
    png = png(file.path(dir, 'gganim_plot%04d.png'), ...),
    jpg =,
    jpeg = jpeg(file.path(dir, 'gganim_plot%04d.jpg'), ...),
    tiff = tiff(file.path(dir, 'gganim_plot%04d.jpg'), ...),
    bmp = bmp(file.path(dir, 'gganim_plot%04d.jpg'), ...)
  )
  if (ref_frame < 0) ref_frame <- nframes_final + 1 + ref_frame
  frame <- plot$scene$get_frame(plot, ref_frame)
  frame <- ggplot_gtable(frame)
  widths_rel <- frame$widths
  widths <- convertWidth(widths_rel, 'mm')
  null_widths <- as.numeric(widths) == 0
  widths[null_widths] <- widths_rel[null_widths]
  heights_rel <- frame$heights
  heights <- convertHeight(heights_rel, 'mm')
  null_heights <- as.numeric(heights) == 0
  heights[null_heights] <- heights_rel[null_heights]

  pb <- progress_bar$new(
    'Rendering [:bar] at :rate fps - eta: :eta',
    total = length(frame_ind)
  )
  pb$tick(0)
  for (i in frame_ind) {
    if (i != frame_ind[1]) grid.newpage()
    frame <- plot$scene$get_frame(plot, i)
    frame <- ggplot_gtable(frame)
    frame$widths <- widths
    frame$heights <- heights
    grid.draw(frame)
    pb$tick(1)
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
