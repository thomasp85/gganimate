#' Renderers provided by gganimate
#'
#' The purpose of the renderer function is to take a list of image files and
#' assemble them into an animation. `gganimate` provide a range of renderers
#' but it is also possible to provide your own, if the supplied ones are lacking
#' in any way. A renderer is given as argument to [animate()]/print() and
#' receives the paths to the individual frames once they have been created.
#'
#' @details The `gifski_renderer()` is used unless otherwise specified in
#' [animate()] or in `options('gganimate.renderer')`. This renderer requires
#' both the `gifski` and `png` packages to be installed.
#'
#' Other possible renderers are:
#' - `magick_renderer()` which requires the `magick` package and produce a `gif`.
#' If `gifski` is not installed, the rendering will be much slower than using the
#' `gifski_renderer()` and can potentially result in system problems when many
#' frames need to be rendered (if `gifski` is installed `magick` will use it
#' under the hood)
#' - `av_renderer()` which requies the `av` package and uses ffmpeg to encode
#' the animation into a video file.
#' - `ffmpeg_renderer()` which requires that ffmpeg has been installed on your
#' computer. As with `av_renderer()` it will use ffmpeg to encode the animation
#' into a video
#' - `sprite_renderer()` which requires `magick` and will render the animation
#' into a spritesheet
#' - `file_renderer()` which has no dependencies and simply returns the
#' animation as a list of image files (one for each frame)
#'
#' It is possible to create your own renderer function providing that it
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
#' - **`magick_renderer`**: Returns a `magick-image` object
#' - **`av_renderer`**: Returns a [video_file] object
#' - **`ffmpeg_renderer`**: Returns a [video_file] object
#' - **`file_renderer`**: Returns a vector of file paths
#'
#' @name renderers
#' @rdname renderers
#'
#' @examples
#' anim <- ggplot(mtcars, aes(mpg, disp)) +
#'   transition_states(gear, transition_length = 2, state_length = 1) +
#'   enter_fade() +
#'   exit_fade()
#'
#' \dontrun{
#' # Renderers are specified in the `animate()` function
#' animate(anim, renderer = sprite_renderer())
#' }
#'
NULL

#' @rdname renderers
#' @export
gifski_renderer <- function(file = NULL, loop = TRUE, width = NULL, height = NULL) {
  check_installed('gifski', 'to use the `gifski_renderer`')
  function(frames, fps) {
    if (is.null(file)) file <- tempfile(fileext = '.gif')
    if (!all(grepl('.png$', frames))) {
      cli::cli_abort('{.pkg gifski} only supports png files', call. = FALSE)
    }
    if (is.null(width) || is.null(height)) {
      dims <- png_dim(frames[1])
      height <- height %||% dims[1]
      width <- width %||% dims[2]
    }
    progress <- interactive()
    if (progress) cli::cli_inform('')
    gif <- gifski::gifski(frames, file, width, height, delay = 1/fps, loop, progress)
    gif_file(gif)
  }
}
#' @rdname renderers
#' @export
file_renderer <- function(dir = '.', prefix = 'gganim_plot', overwrite = FALSE) {
  function(frames, fps) {
    if (!dir.exists(dir)) dir.create(dir, showWarnings = FALSE, recursive = TRUE)
    new_names <- file.path(dir, sub('gganim_plot', prefix, basename(frames)))
    if (any(!file.copy(frames, new_names, overwrite = overwrite))) {
      cli::cli_warn('{.fun file_renderer} failed to copy frames to the destination directory')
    }
    invisible(new_names)
  }
}
#' @rdname renderers
#' @export
av_renderer <- function(file = NULL, vfilter = "null", codec = NULL, audio = NULL) {
  check_installed('av', 'to use the `av_renderer`')
  def_ext <- if (.Platform$GUI == "RStudio" && "libvpx" %in% av::av_encoders()$name) ".webm" else ".mp4"
  function(frames, fps) {
    if (is.null(file)) {
      file <- tempfile(fileext = def_ext)
    }
    progress <- interactive()
    av::av_encode_video(input = frames, output = file, framerate = fps,
                        vfilter = vfilter, codec = codec, audio = audio,
                        verbose = progress)
    video_file(file)
  }
}
has_ffmpeg <- function(ffmpeg = 'ffmpeg') {
  try_fetch(
    {
      suppressWarnings(system2(ffmpeg, '-version', stdout = FALSE, stderr = FALSE))
      TRUE
    },
    error = function(e) {
      FALSE
    }
  )
}
#' @rdname renderers
#' @export
ffmpeg_renderer <- function(format = 'auto', ffmpeg = NULL, options = list(pix_fmt = 'yuv420p')) {
  ffmpeg <- ffmpeg %||% 'ffmpeg'
  if (!has_ffmpeg(ffmpeg)) cli::cli_abort('The ffmpeg library is not available at the specified location')
  if (format == 'auto') {
    format <- if (.Platform$GUI == "RStudio" &&
                  any(grepl('--enable-libvpx', system2(ffmpeg, '-version', stdout = TRUE)))) {
      "webm"
    } else {
      "mp4"
    }
  }
  if (is_list(options)) {
    if (is.null(names(options))) {
      cli::cli_abort('{.arg options} must be a named list')
    }
    opt_name <- paste0('-', sub('^-', '', names(options)))
    opts <- vapply(options, paste, character(1), collapse = ' ')
    options <- paste(paste0(opt_name, ' ', opts), collapse = ' ')
  }
  check_character(options)

  function(frames, fps) {
    progress <- interactive()
    output_file <- tempfile(fileext = paste0('.', sub('^\\.', '', format)))
    frame_loc <- dirname(frames[1])
    file_glob <- sub('^.*(\\..+$)', 'gganim_plot%4d\\1', basename(frames[1]))
    file_glob <- file.path(frame_loc, file_glob)
    system2(ffmpeg, c("-pattern_type sequence",
      paste0('-r ', fps),
      paste0('-i ', file_glob),
      '-y',
      '-loglevel ', if (progress) 'info' else 'quiet',
      paste0('-r ', fps),
      '-hide_banner',
      options,
      output_file
    ))
    if (!file.exists(output_file)) cli::cli_abort('Rendering with ffmpeg failed to produce an output file')
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
  check_installed('magick', 'to use the `magick_renderer`')
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
  check_installed('magick', 'to use the `sprite_renderer`')
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
#' @param f a ‘factor’ in the sense that [as.factor(f)][base::factor] defines
#' the grouping, or a list of such factors in which case their interaction is
#' used for the grouping.
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
  check_string(file, allow_empty = FALSE)
  if (!grepl('.gif$', file)) cli::cli_abort('{.arg file} must point to a gif file')
  class(file) <- 'gif_image'
  file
}
#' @rdname gif_file
#' @export
print.gif_image <- function(x, ...) {
  viewer <- getOption("viewer", utils::browseURL)
  if (is_function(viewer) && length(x)) {
    viewer(x)
  } else {
    invisible()
  }
}
#' @rdname gif_file
#' @export
knit_print.gif_image <- function(x, options, ...) {
  knitr_path <- knitr::fig_path('.gif', options)
  dir.create(dirname(knitr_path), showWarnings = FALSE, recursive = TRUE)
  file.copy(x, knitr_path, overwrite = TRUE)
  if (is.null(options$out.width)) {
    options$out.width <- options$fig.width * options$dpi / (options$fig.retina %||% 1)
  }
  knitr::knit_print(knitr::include_graphics(knitr_path), options, ...)
}
#' @rdname gif_file
#' @export
split.gif_image <- function(x, f, drop = FALSE, ...) {
  check_installed('magick', 'to split gif files')
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
#' @param width The width the output should be scaled to
#' @param ... Arguments passed on
#'
#' @return `video_file` returns a `video_file` object which is a shallow wrapper
#' around the file path text string.
#'
#' @keywords internal
#' @export
#'
video_file <- function(file) {
  check_string(file, allow_empty = FALSE)
  class(file) <- 'video_file'
  file
}
#' @rdname video_file
#' @export
print.video_file <- function(x, width = NULL, ...) {
  if (grepl('\\.(mp4)|(webm)|(ogg)$', x, ignore.case = TRUE)) {
    if (grepl('\\.mp4$', x, ignore.case = TRUE) && .Platform$GUI == "RStudio") {
      utils::browseURL(x)
    } else {
      print(htmltools::browsable(as_html_video(x, width, ...)))
    }
  } else {
    viewer <- getOption("viewer", utils::browseURL)
    viewer(x)
  }
}
#' @rdname video_file
#' @export
knit_print.video_file <- function(x, options, ...) {
  if (grepl('\\.(mp4)|(webm)|(ogg)$', x, ignore.case = TRUE)) {
    knitr::knit_print(
      htmltools::browsable(
        as_html_video(
          x,
          width = get_chunk_width(options),
          autoplay = get_chunk_autoplay(options),
          muted = get_chunk_muted(options),
          loop = get_chunk_loop(options),
          controls = get_chunk_controls(options)
        )
      ),
      options,
      ...
    )
  } else {
    cli::cli_warn('The video format doesn\'t support HTML')
    invisible(NULL)
  }
}
#' @export
split.video_file <- function(x, f, drop = FALSE, ...) {
  cli::cli_abort('{.cls video_file} objects does not support splitting')
}
as_html_video <- function(x, width = NULL, autoplay = TRUE, muted = FALSE, loop = FALSE, controls = TRUE, ...) {
  check_installed('base64enc', 'for showing video')
  check_installed('htmltools', 'for showing video')
  format <- tolower(sub('^.*\\.(.+)$', '\\1', x))
  htmltools::HTML(paste0(
    '<video',
    if (autoplay) ' autoplay' else '',
    if (muted) ' muted' else '',
    if (loop) ' loop' else '',
    if (controls) ' controls' else '',
    if (is.null(width)) '' else paste0(' width="', width, '"'),
    '>',
    '<source src="data:video/', format, ';base64,', base64enc::base64encode(x), '" type="video/mp4"></video>'
  ))
}
get_chunk_autoplay <- function(options) {
  options$gganimate$autoplay %||% TRUE
}
get_chunk_muted <- function(options) {
  options$gganimate$muted %||% FALSE
}
get_chunk_loop <- function(options) {
  options$gganimate$loop %||% FALSE
}
get_chunk_controls <- function(options) {
  options$gganimate$controls %||% TRUE
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
  check_string(file, allow_empty = FALSE)
  attributes(file) <- list(fps = fps, single_width = width, full_width = full_width, height = height)
  class(file) <- 'sprite_image'
  file
}
#' @rdname sprite_file
#' @export
print.sprite_image <- function(x, width = NULL, ...) {
  print(htmltools::browsable(as_sprite_html(x, width = width)))
}
#' @rdname sprite_file
#' @export
knit_print.sprite_image <- function(x, options, ...) {
  knitr::knit_print(htmltools::browsable(as_sprite_html(x, width = get_chunk_width(options))), options, ...)
}
#' @importFrom glue glue
as_sprite_html <- function(x, width = NULL, ...) {
  check_installed('base64enc', 'for showing sprites')
  check_installed('htmltools', 'for showing sprites')
  # Sprite animation code inspired by https://medium.com/dailyjs/how-to-build-a-simple-sprite-animation-in-javascript-b764644244aa
  sprite_id <- sample(1e6, 1)
  css <- glue(
    '
    <style>
    #sprite-%sprite_id% {
      padding-bottom: %height%;
      width: %width%;
      background: url(data:image/png;base64,%img_encode%) 0px 0px;
      background-size: %size%;
    }
    </style>
    ',
    height = paste0(100*attr(x, 'height') / attr(x, 'single_width'), '%'),
    width = width %||% paste0(attr(x, 'single_width'), 'px'),
    size = paste0(100 * attr(x, 'full_width') / attr(x, 'single_width'), '%'),
    img_encode = base64enc::base64encode(x),
    .open = '%',
    .close = '%'
  )
  html <- glue('<div class="gganimate-sprite"><div id="sprite-{sprite_id}" onclick="toggleAnimation_{sprite_id}()"></div></div>')
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
        document.getElementById("sprite-%sprite_id%").style.backgroundPosition = `${position_%sprite_id%}%% 0%%`;

        if (position_%sprite_id% < 100) {
          position_%sprite_id% = position_%sprite_id% + diff;
        } else {
          position_%sprite_id% = %width%;
        }

      }, interval);
    }

    animateScript_%sprite_id%()
    </script>
    ',
    width = 100 / (attr(x, 'full_width') / attr(x, 'single_width') - 1),
    fps = attr(x, 'fps'),
    .open = '%',
    .close = '%'
  )
  full <- paste0(css, '\n', html, '\n', js)
  htmltools::HTML(full)
}
#' @export
split.sprite_image <- function(x, f, drop = FALSE, ...) {
  cli::cli_abort('sprite_image objects does not support splitting')
}


in_pkgdown <- function() {
  identical(Sys.getenv("IN_PKGDOWN"), "true")
}

get_chunk_width <- function(options) {
  options$out.width %||% paste0((options$fig.width / (options$fig.retina %||% 1)), 'px')
}
