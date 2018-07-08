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
#' @param dir The directory to copy the frames to
#' @param prefix The filename prefix to use for the image files
#' @param overwrite Logical. If TRUE, existing files will be overwritten.
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
#' @importFrom magick image_read image_animate image_read_svg
#' @export
magick_renderer <- function(loop = TRUE) {
  function(frames, fps) {
    anim <- if (grepl('.svg$', frames[1])) {
      image_read_svg(frames)
    } else {
      image_read(frames)
    }
    anim <- image_animate(anim, fps, loop = if (loop) 0 else 1)
    anim
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
