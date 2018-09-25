#' Renderers provided by gganimate
#'
#' The purpose of the renderer function is to take a list of image files and
#' assemble them into an animation. `gganimate` provide a range of renderers
#' but it is also possible to provide your own, if the supplied ones are lacking
#' in any way. A renderer is given as argument to [animate()]/print() and
#' receives the paths to the individual frames once they have been created.
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
#' @param vfilter A string defining an ffmpeg filter graph. This is the same
#' parameter as the `-vf` argument in the `ffmpeg` command line utility.
#' @param codec The name of the video codec. The default is `libx264` for most
#' formats, which usually the best choice. See the `av` documentation for more
#' information.
#' @param audio An optional file with sounds to add to the video
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
#' - **`av_renderer`**: Returns a [video_file] object
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
av_renderer <- function(file = tempfile(fileext = '.mp4'), vfilter = "null", codec = NULL, audio = NULL) {
  if (!requireNamespace('av', quietly = TRUE)) {
    stop('The av package is required to use this renderer', call. = FALSE)
  }
  function(frames, fps) {
    progress <- !isTRUE(getOption("knitr.in.progress"))
    av::av_encode_video(input = frames, output = file, framerate = fps,
                        vfilter = vfilter, codec = codec, audio = audio,
                        verbose = progress)
    video_file(file)
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
#' @rdname renderers
#' @export
sprite_renderer <- function() {
  if (!requireNamespace('magick', quietly = TRUE)) {
    stop('The magick package is required to use this renderer', call. = FALSE)
  }
  function(frames, fps) {
    sprite <- if (grepl('.svg$', frames[1])) {
      magick::image_read_svg(frames)
    } else {
      magick::image_read(frames)
    }
    single_dim <- magick::image_info(sprite[1])
    sprite <- magick::image_append(sprite)
    full_dim <- magick::image_info(sprite)
    file <- tempfile(fileext = '.png')
    magick::image_write(sprite, file, 'png')
    sprite_file(file, fps, width = single_dim$width, full_width = full_dim$width, height = single_dim$height)
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
#' @param ... Arguments passed on
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
  viewer <- getOption("viewer", utils::browseURL)
  if (is.function(viewer) && length(x)) {
    viewer(x)
  } else {
    invisible()
  }
}
#' @rdname gif_file
#' @export
knit_print.gif_image <- function(x, options, ...) {
  knitr_path <- knitr::fig_path('.gif')
  dir.create(dirname(knitr_path), showWarnings = FALSE, recursive = TRUE)
  file.copy(x, knitr_path, overwrite = TRUE)
  knitr::knit_print(knitr::include_graphics(knitr_path), options, ...)
}
#' @rdname gif_file
#' @export
split.gif_image <- function(x, f, drop = FALSE, ...) {
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
#' @param ... Arguments passed on
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
    print(htmltools::browsable(as_html_video(x)))
  } else {
    viewer <- getOption("viewer", utils::browseURL)
    viewer(x)
  }
}
#' @rdname video_file
#' @export
knit_print.video_file <- function(x, options, ...) {
  if (grepl('\\.(mp4)|(webm)|(ogg)$', x, ignore.case = TRUE)) {
    knitr::knit_print(htmltools::browsable(as_html_video(x)), options, ...)
  } else {
    warning('The video format doesn\'t support HTML', call. = FALSE)
    invisible(NULL)
  }
}
#' @export
split.video_file <- function(x, f, drop = FALSE, ...) {
  stop('video_file objects does not support splitting', call. = FALSE)
}
as_html_video <- function(x) {
  if (!requireNamespace("base64enc", quietly = TRUE)) {
    stop('The base64enc package is required for showing video')
  }
  if (!requireNamespace("htmltools", quietly = TRUE)) {
    stop('The htmltools package is required for showing video')
  }
  format <- tolower(sub('^.*\\.(.+)$', '\\1', x))
  htmltools::HTML(paste0(
    '<video controls autoplay><source src="data:video/',
    format,
    ';base64,',
    base64enc::base64encode(x),
    '" type="video/mp4"></video>'
  ))
}
#' Wrap an image sprite for easy handling
#'
#' This function is equivalent to [gif_file()] but works for animations encoded
#' as a sprite. A sprite is a single image file where each frame of the
#' animation is stacked next to each other. The animation then happens by
#' changing what slice of the image is shown. The implementation used allow
#' users to click on the animation in order to toggle pause/play.
#'
#' @param file A png file with frames placed horizontally
#' @param fps The framerate for the sprite animation
#' @param width,height The dimension of a single frame
#' @param full_width The width of the whole image
#' @param x A `sprite_image` object
#' @param ... Arguments passed on
#'
#' @return `sprite_file` returns a `sprite_image` object which is a shallow wrapper
#' around the file path text string along with dimensions and fps used for
#' animating the sprite when printing.
#'
#' @keywords internal
#' @export
#'
sprite_file <- function(file, fps, width, full_width, height) {
  stopifnot(length(file) == 1)
  attributes(file) <- list(fps = fps, single_width = width, full_width = full_width, height = height)
  class(file) <- 'sprite_image'
  file
}
#' @rdname sprite_file
#' @export
print.sprite_image <- function(x, ...) {
  print(htmltools::browsable(as_sprite_html(x)))
}
#' @rdname sprite_file
#' @export
knit_print.sprite_image <- function(x, options, ...) {
  knitr::knit_print(htmltools::browsable(as_sprite_html(x)), options, ...)
}
#' @importFrom glue glue
as_sprite_html <- function(x, ...) {
  if (!requireNamespace("base64enc", quietly = TRUE)) {
    stop('The base64enc package is required for showing video')
  }
  if (!requireNamespace("htmltools", quietly = TRUE)) {
    stop('The htmltools package is required for showing video')
  }
  # Sprite animation code inspired by https://medium.com/dailyjs/how-to-build-a-simple-sprite-animation-in-javascript-b764644244aa
  sprite_id <- sample(1e6, 1)
  css <- glue(
    '
    <style>
    #sprite-%sprite_id% {
      height: %height%px;
      width: %width%px;
      background: url(data:image/png;base64,%img_encode%) 0px 0px;
    }
    </style>
    ',
    height = attr(x, 'height'),
    width = attr(x, 'single_width'),
    img_encode = base64enc::base64encode(x),
    .open = '%',
    .close = '%'
  )
  html <- glue('<div class="gganimate-sprite"><p id="sprite-{sprite_id}" onclick="toggleAnimation_{sprite_id}()"></p></div>')
  js <- glue(
    '
    <script>
    var tID_%sprite_id%;
    var running_%sprite_id% = true;
    var position_%sprite_id% = %width%;

    function toggleAnimation_%sprite_id%() {
      if (running_%sprite_id%) {
        clearInterval(tID_%sprite_id%);
      } else {
        animateScript_%sprite_id%();
      }
      running_%sprite_id% = !running_%sprite_id%;
    }

    function animateScript_%sprite_id%() {
      const interval = 1/%fps% * 1000; //100 ms of interval for the setInterval()
      const diff = %width%; //diff as a variable for position offset

      tID_%sprite_id% = setInterval(() => {
        document.getElementById("sprite-%sprite_id%").style.backgroundPosition = `-${position_%sprite_id%}px 0px`;

        if (position_%sprite_id% < %full_width%) {
          position_%sprite_id% = position_%sprite_id% + diff;
        } else {
          position_%sprite_id% = %width%;
        }

      }, interval);
    }

    animateScript_%sprite_id%()
    </script>
    ',
    width = attr(x, 'single_width'),
    full_width = attr(x, 'full_width'),
    fps = attr(x, 'fps'),
    .open = '%',
    .close = '%'
  )
  full <- paste0(css, '\n', html, '\n', js)
  htmltools::HTML(full)
}
#' @export
split.sprite_image <- function(x, f, drop = FALSE, ...) {
  stop('sprite_image objects does not support splitting', call. = FALSE)
}
