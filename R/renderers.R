#' Renderers provided by gganimate
#'
#' The purpose of the renderer function is to take a list of image files and
#' assemble them into an animation. `gganimate` provide a range of renderers
#' but it is also possible to provide your own, if the supplied ones are lacking
#' in any way. A renderer is given as argument to [animate()]/print() and
#' recieves the paths to the individual frames once they have been created.
#'
#' @details It is possible to provide your own renderer function providing that it
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
#' take the dimensions from the frame, otherwise it will rescale it.
#' @param format The video format to encode the animation into
#' @param ffmpeg The location of the `ffmpeg` executable. If `NULL` it will be
#' assumed to be on the search path
#' @param options Either a character vector of command line options for ffmpeg
#' or a named list of option-value pairs that will be converted to command line
#' options automatically
#'
#' @return The provided renderers are factory functions that returns a new function
#' that take `frames` and `fps` as arguments, the former being a character
#' vector with file paths to the images holding the separate frames, in the
#' order they should appear, and the latter being the framerate to use for the
#' animation in frames-per-second.
#'
#' The return type of the different returned renderers are:
#' - **`gifski_renderer`**: Returns a [gif_image] object
#' - **`file_renderer`**: Returns a vector of file paths
#' - **`ffmpeg_renderer`**: Returns a [video_file] object
#' - **`magick_renderer`**: Returns a `magick-image` object
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
ffmpeg_renderer <- function(format = 'mp4', ffmpeg = NULL, options = list(pix_fmt = 'yuv420p')) {
  ffmpeg <- ffmpeg %||% 'ffmpeg'
  tryCatch(
    suppressWarnings(system2(ffmpeg, '-version', stdout = FALSE, stderr = FALSE)),
    error = function(e) {
      stop('The ffmpeg library is not available at the specified location', call. = FALSE)
    }
  )
  if (is.list(options)) {
    if (is.null(names(options))) {
      stop('options list must be named', call. = FALSE)
    }
    opt_name <- paste0('-', sub('^-', '', names(options)))
    opts <- vapply(options, paste, character(1), collapse = ' ')
    options <- paste(paste0(opt_name, ' ', opts), collapse = ' ')
  }
  stopifnot(is.character(options))

  function(frames, fps) {
    output_file <- tempfile(fileext = paste0('.', sub('^\\.', '', format)))
    frame_loc <- dirname(frames[1])
    file_glob <- sub('^.*(\\..+$)', '%*\\1', basename(frames[1]))
    file_glob <- file.path(frame_loc, file_glob)
    system2(ffmpeg, c(
      paste0('-i ', file_glob),
      '-y',
      '-loglevel quiet',
      paste0('-framerate ', 1/fps),
      '-hide_banner',
      options,
      output_file
    ))
    if (format == 'gif') {
      gif_file(output_file)
    } else {
      video_file(output_file)
    }
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
#' @aliases gif_image
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
  if (isTRUE(getOption("knitr.in.progress"))) {
    knitr_path <- knitr::fig_path('.gif')
    file.copy(x, knitr_path, overwrite = TRUE)
    knitr::include_graphics(knitr_path)
  } else if (is.function(viewer) && length(x)) {
    viewer <- getOption("viewer")
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
#' Wrap a video file for easy handling
#'
#' This function is equivalent to [gif_file()] but works for arbitrary video
#' file formats. There are some caveats involved though, most notably it doesn't
#' currently supports splitting so you can't easily use this output together
#' with [split_animation()]. Graceful printing is contingent on the file format.
#' Only `mp4`, `webm`, and `ogg` is supported by the HTML format and can thus be
#' shown in the RStudio viewer and inside HTML documents created with knitr.
#' Other formats will be opened in their default OS-specific viewer.
#'
#' @param file A video file
#' @param x A `video_file` object
#'
#' @return `video_file` returns a `video_file` object which is a shallow wrapper
#' around the file path text string.
#'
#' @keywords internal
#' @export
#'
video_file <- function(file) {
  stopifnot(length(file) == 1)
  class(file) <- 'video_file'
  file
}
#' @rdname video_file
#' @export
print.video_file <- function(x, ...) {

  if (grepl('\\.(mp4)|(webm)|(ogg)$', x, ignore.case = TRUE)) {
    if (!requireNamespace("base64enc", quietly = TRUE)) {
      stop('The base64enc package is required for showing video')
    }
    if (!requireNamespace("base64enc", quietly = TRUE)) {
      stop('The base64enc package is required for showing video')
    }
    format <- tolower(sub('^.*\\.(.+)$', '\\1', x))
    html <- paste0(
      '<video controls autoplay><source src="data:video/',
      format,
      ';base64,',
      base64enc::base64encode(x),
      '" type="video/mp4"></video>'
    )
    print(htmltools::browsable(htmltools::HTML(html)))
  } else {
    viewer <- getOption("viewer")
    viewer(x)
  }
}
#' @export
split.video_file <- function(x, f, drop = FALSE, ...) {
  stop('video_file objects does not support splitting', call. = FALSE)
}
