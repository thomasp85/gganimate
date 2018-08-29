#' Render a gganim object
#'
#' This function takes a gganim object and renders it into an animation. The
#' nature of the animation is dependent on the renderer, but defaults to using
#' `magick` to render it to a gif. The length and framerate is decided on render
#' time and can be any two combination of `nframes`, `fps`, and `length`.
#' Rendering is happening in discrete time units. This means that any event in
#' the animation is rounded of to the nearest frame (e.g. entering will always
#' take a whole number of frames). This means that rounding artifacts are
#' possible when only rendering few frames. To avoid this you can increase the
#' `detail` argument. `detail` will get multiplied to `nframes` and the
#' resulting number of frames will get calculated, but only `nframes` evenly
#' spaced frames are rendered.
#'
#' @param plot,x A `gganim` object
#' @param nframes The number of frames to render
#' @param fps The framerate of the animation in frames/sec
#' @param length The length of the animation in seconds
#' @param detail The number of additional frames to calculate, per frame
#' @param renderer The function used to render the generated frames into an
#' animation. Gets a vector of paths to images along with the framerate.
#' @param device The device to use for rendering the single frames. Possible
#' values are `'png'`, `'jpeg'`, `'tiff'`, `'bmp'`, `'svg'`, and `'svglite'`.
#' Defaults to `'png'`. (`'svglite'` requires the svglite package).
#' @param ref_frame The frame to use for fixing dimensions of the plot, e.g. the
#' space available for axis text. Defaults to the first frame. Negative values
#' counts backwards (-1 is the last frame)
#' @param ... Arguments passed on to the device
#'
#' @return The return value of the `renderer` function
#'
#' @details  `print.gganim`()  is an alias for `animate()` in the same way as
#' `print.ggplot()` is an alias for `plot.ggplot()`. This ensures that gganimate
#' behaves ggplot2-like and produces the animation when the object is printed.
#' The `plot()` method is different and produces an ensemble of frames to give
#' a static overview of the animation. The default is to produce a 3x3 grid.
#'
#' @importFrom grid grid.newpage grid.draw convertWidth convertHeight
#' @importFrom grDevices png jpeg tiff bmp svg dev.off
#' @importFrom progress progress_bar
#' @importFrom ggplot2 ggplot_gtable ggplot_build
#' @export
animate <- function(plot, nframes = 100, fps = 10, length = NULL, detail = 1,
                    renderer = gifski_renderer(), device = 'png', ref_frame = 1,
                    ...) {
  if (sum(c(is.null(nframes), is.null(fps), is.null(length))) > 1) {
    stop("At least 2 of 'nframes', 'fps', and 'length' must be given", call. = FALSE)
  }
  if (device == 'svglite' && !requireNamespace('svglite', quietly = TRUE)) {
    stop('The svglite package is required to use this device', call. = FALSE)
  }

  nframes <- nframes %||% round(length * fps)
  fps <- fps %||% round(nframes / length)
  nframes_total <- (nframes - 1) * detail + 1
  plot <- prerender(plot, nframes_total)
  nframes_final <- get_nframes(plot)

  frame_ind <- unique(round(seq(1, nframes_final, length.out = nframes)))
  if (nframes != length(frame_ind)) message('nframes adjusted to match plot')

  if (ref_frame < 0) ref_frame <- nframes_final + 1 + ref_frame

  frames_vars <- draw_frames(plot, frame_ind, device, ref_frame, ...)

  animation <- renderer(frames_vars$frame_source, fps)
  attr(animation, 'frame_vars') <- frame_vars
  set_last_animation(animation)
  animation
}
# Build plot for a specific number of frames
prerender <- function(plot, nframes) {
  plot <- set_nframes(plot, nframes)
  ggplot_build(plot)
}
# Draw each frame as an image based on a specified device
# Returns a data.frame of frame metadata with image location in frame_source
# column
draw_frames <- function(plot, frames, device, ref_frame, ...) {
  dims <- plot_dims(plot, ref_frame)

  dir <- tempfile(pattern = '')
  dir.create(dir, showWarnings = FALSE)
  files <- file.path(dir, sprintf('gganim_plot%04d', seq_along(frames)))
  files <- switch(
    tolower(device),
    png = paste0(files, '.png'),
    jpg =,
    jpeg = paste0(files, '.jpg'),
    tiff = paste0(files, '.tif'),
    bmp = paste0(files, '.bmp'),
    svglite =,
    svg = paste0(files, '.svg'),
    stop('Unsupported device', call. = FALSE)
  )
  device <- switch(
    device,
    png = png,
    jpg =,
    jpeg = jpeg,
    tiff = tiff,
    bmp = bmp,
    svg = svg,
    svglite = svglite::svglite
  )

  pb <- progress_bar$new(
    'Rendering [:bar] at :fps fps ~ eta: :eta',
    total = length(frames)
  )
  start <- Sys.time()
  pb$tick(0)

  for (i in seq_along(frames)) {
    device(files[i], ...)

    frame <- plot$scene$get_frame(plot, frames[i])
    frame <- ggplot_gtable(frame)
    frame$widths <- dims$widths
    frame$heights <- dims$heights
    grid.draw(frame)

    rate <- i/as.double(Sys.time() - start, units = 'secs')
    if (is.nan(rate)) rate <- 0
    rate <- format(rate, digits = 2)
    pb$tick(tokens = list(fps = rate))

    dev.off()
  }

  frame_vars <- plot$scene$frame_vars[frames, , drop = FALSE]
  frame_vars$frame_source <- files
  frame_vars
}
# Get dimensions of plot based on a reference frame
plot_dims <- function(plot, ref_frame) {
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
  list(widths = widths, heights = heights)
}
