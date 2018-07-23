#' Renderers provided by gganimate
#'
#' The purpose of the renderer function is to take a list of image files and
#' assemble them into an animation. `gganimate` provide a range of renderers
#' but it is also possible to provide your own, if the supplied ones are lacking
#' in any way. A renderer is given as argument to [animate()]/print() and
#' recieves the paths to the individual frames once they have been created.
#'
#' @details The return value of a rendering is dependent on the specific
#' renderer. `magick_renderer()` returns a `magick-image` invisibly and displays
#' the animation in the viewer. `file_renderer()` returns the path to the image
#' files invisibly and does not display anything.
#'
#' It is possible to provide your own renderer function providing that it
#' matches the required signature (`frames` and `fps` argument). The return
#' value of your provided function will be the return value ultimately given by
#' [animate()]
#'
#' @param loop Logical. Should the produced gif loop
#' @param file The animation file
#' @param dir The directory to copy the frames to
#' @param prefix The filename prefix to use for the image files
#' @param overwrite Logical. If TRUE, existing files will be overwritten.
#' @param width,height Dimensions of the animation in pixels. If `NULL` will
#' tale the dimensions from the frame, otherwise it will rescale it.
#'
#' @return The provided renderers are factory functions that returns a new function
#' that take `frames` and `fps` as arguments, the former being a character
#' vector with file paths to the images holding the separate frames, in the
#' order they should appear, and the latter being the framerate to use for the
#' animation in frames-per-second.
#'
#' @name renderers
#' @rdname renderers
NULL

#' @rdname renderers
#' @importFrom png readPNG
#' @importFrom gifski gifski
#' @export
gifski_renderer <- function(file = tempfile(fileext = '.gif'), loop = TRUE, width = NULL, height = NULL) {
  function(frames, fps) {
    if (!all(grepl('.png$', frames))) {
      stop('gifski only supports png files', call. = FALSE)
    }
    if (is.null(width) || is.null(height)) {
      dims <- dim(readPNG(frames[1], native = TRUE))
      height <- height %||% dims[1]
      width <- width %||% dims[2]
    }
    progress <- !isTRUE(getOption("knitr.in.progress"))
    if (progress) message('')
    gif <- gifski(frames, file, width, height, delay = 1/fps, loop, progress)
    gif_file(gif)
  }
}
#' @rdname renderers
#' @export
file_renderer <- function(dir = '~', prefix = 'gganim_plot', overwrite = FALSE) {
  function(frames, fps) {
    if (!dir.exists(dir)) dir.create(dir, showWarnings = FALSE, recursive = TRUE)
    new_names <- file.path(dir, sub('gganim_plot', prefix, basename(frames)))
    file.copy(frames, new_names, overwrite = overwrite)
    invisible(new_names)
  }
}
#' @rdname renderers
#' @export
magick_renderer <- function(loop = TRUE) {
  if (!requireNamespace('magick', quietly = TRUE)) {
    stop('The magick package is required to use this renderer', call. = FALSE)
  }
  function(frames, fps) {
    anim <- if (grepl('.svg$', frames[1])) {
      magick::image_read_svg(frames)
    } else {
      magick::image_read(frames)
    }
    anim <- magick::image_animate(anim, fps, loop = if (loop) 0 else 1)
    anim
  }
}


# HELPERS -----------------------------------------------------------------

#' Wrap a gif file for easy handling
#'
#' This is a simple class for gif files that takes care printing the file
#' correctly in different environment (e.g. knitr, RStudio, etc.). If your
#' renderer produces a gif file you can wrap the final output in `gif_file` to
#' get all of these benefits for free.
#'
#' @param file The gif file to be wrapped
#' @param x A `gif_image` object
#' @inheritParams base::split
#'
#' @return `gif_file` returns a `gif_image` object. `split` returns a list of
#' `magick-image` objects and requires magick to work.
#'
#' @keywords internal
#' @export
#'
gif_file <- function(file) {
  stopifnot(length(file) == 1)
  if (!grepl('.gif$', file)) stop('file must be a gif', call. = FALSE)
  class(file) <- 'gif_image'
  file
}
#' @rdname gif_file
#' @export
print.gif_image <- function(x, ...) {
  viewer <- getOption("viewer")
  viewer_supported <- c("bmp", "png", "jpeg", "jpg", "svg",
                        "gif", "webp")
  if (isTRUE(getOption("knitr.in.progress"))) {
    knitr_path <- knitr::fig_path('.gif')
    file.copy(x, knitr_path, overwrite = TRUE)
    knitr::include_graphics(knitr_path)
  } else if (is.function(viewer) && length(x)) {
    viewer(x)
  } else {
    invisible()
  }
}
#' @rdname gif_file
#' @export
split.gif_file <- function(x, f, drop = FALSE, ...) {
  if (!requireNamespace('magick', quietly = TRUE)) {
    stop('Splitting gifs require the magick package', call. = FALSE)
  }
  gif <- magick::image_read(x)
  split(gif, f, drop = drop, ...)
}
