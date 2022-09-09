#' Render a gganim object
#'
#' This function takes a gganim object and renders it into an animation. The
#' nature of the animation is dependent on the renderer, but defaults to using
#' `gifski` to render it to a gif. The length and framerate is decided on render
#' time and can be any two combination of `nframes`, `fps`, and `duration`.
#' Rendering is happening in discrete time units. This means that any event in
#' the animation is rounded of to the nearest frame (e.g. entering will always
#' take a whole number of frames). This means that rounding artifacts are
#' possible when only rendering few frames. To avoid this you can increase the
#' `detail` argument. `detail` will get multiplied to `nframes` and the
#' resulting number of frames will get calculated, but only `nframes` evenly
#' spaced frames are rendered.
#'
#' @param plot,x A `gganim` object
#' @param nframes The number of frames to render (default `100`)
#' @param fps The framerate of the animation in frames/sec (default `10`)
#' @param duration The length of the animation in seconds (unset by default)
#' @param detail The number of additional frames to calculate, per frame (default `1`)
#' @param renderer The function used to render the generated frames into an
#' animation. Gets a vector of paths to images along with the framerate. (by
#' default it will use [gifski_renderer()] if gifski is installed. If not it
#' will use [magick_renderer()] if magick is installed and then [av_renderer()]
#' if av is installed. If all fails it will use the [file_renderer()])
#' @param device The device to use for rendering the single frames. Possible
#' values are `'png'`, `'ragg_png'` (requires the ragg package), `'jpeg'`,
#' `'tiff'`, `'bmp'`, `'svg'`, and `'svglite'` (requires the svglite package).
#' (default `'png'`)
#' @param ref_frame The frame to use for fixing dimensions of the plot, e.g. the
#' space available for axis text. Defaults to the first frame. Negative values
#' counts backwards (-1 is the last frame) (default `1`)
#' @param start_pause,end_pause Number of times to repeat the first and last
#' frame in the animation (default is `0` for both)
#' @param rewind Should the animation roll back in the end (default `FALSE`)
#' @param ... Arguments passed on to the device.
#' For available device arguments, see [grDevices::png()] or [grDevices::svg()]
#'
#' @return The return value of the [renderer][renderers] function
#'
#' @details  `print.gganim`()  is an alias for `animate()` in the same way as
#' `print.ggplot()` is an alias for `plot.ggplot()`. This ensures that gganimate
#' behaves ggplot2-like and produces the animation when the object is printed.
#' The `plot()` method is different and produces a single frame for inspection
#' (by default frame 50 out of 100).
#'
#' Animations can be saved to disk using [anim_save()] in much the same way
#' [ggsave()][ggplot2::ggsave] works for static plots.
#'
#' @section Defaults:
#' It is possible to overwrite the defaults used by gganimate for the animation
#' by setting them with [options()] (prefixed with `gganimate.`. As an example,
#' if you would like to change the default nframes to 50 you would call
#' `options(gganimate.nframes = 50)`. In order to set default device arguments
#' (those you would normally pass through with `...`) you should use the
#' `gganimate.dev_args` options and provide a list of arguments e.g.
#' `options(gganimate.dev_args = list(width = 800, height = 600))` Defaults set
#' this way can still be overridden by giving arguments directly to `animate()`.
#'
#' **knitr Support:** \cr
#' It is possible to specify the arguments to `animate()` in the chunk options
#' when using `gganimate` with `knitr`. Arguments specified in this way will
#' have precedence over defaults, but not over arguments specified directly in
#' `animate()`. The arguments should be provided as a list to the `gganimate`
#' chunk option, e.g. `{r, gganimate = list(nframes = 50, fps = 20)}`. A few
#' build-in knitr options have relevance for animation and will be used unless
#' given specifically in the `gganimate` list option. The native knitr options
#' supported are:
#'
#' - `dev`: will set `device`
#' - `dev.args`: will set additional arguments to the device (`...`)
#' - `fig.width`, `fig.height`, `fig.asp`, `fig.dim`: will set `width` and
#'   `height` of the device.
#'
#' @section Label variables:
#' All plots have a certain set of variables available for string literal
#' interpolation within plot labels. These are:
#'
#' - **frame** gives you the frame index for the current frame
#' - **nframes** gives you the total number of frames in the animation
#' - **progress** gives you the progress of the animation at the current frame
#'   (equal to `frame/nframes`)
#' - **data** gives you the layer data for the current frame (as a list of data
#'   frames)
#'
#' Further, the transition and view in use can also make variables available.
#' Consult the documentation for these for more detail.
#'
#' @importFrom grid grid.newpage grid.draw convertWidth convertHeight
#' @importFrom grDevices png jpeg tiff bmp svg dev.off
#' @importFrom progress progress_bar
#' @importFrom ggplot2 ggplot_gtable ggplot_build
#' @export
#'
#' @examples
#' anim <- ggplot(mtcars, aes(mpg, disp)) +
#'   geom_point(aes(color = gear)) +
#'   transition_states(gear, transition_length = 2, state_length = 1) +
#'   enter_fade() +
#'   exit_fade()
#'
#' \dontrun{
#' # Explicitly animate using default (same as just printing the animation)
#' animate(anim)
#'
#' # Change duration and framerate
#' animate(anim, fps = 20, duration = 15)
#'
#' # Make the animation pause at the end and then rewind
#' animate(anim, nframes = 100, end_pause = 10, rewind = TRUE)
#'
#' # Use a different renderer
#' animate(anim, renderer = file_renderer('~/animation/'))[1:6]
#'
#' # Specify device dimensions and/or resolution
#' animate(anim, height = 2, width = 3, units = "in", res = 150)
#' }
#'
animate <- function(plot, ...) {
  UseMethod('animate')
}
#' @export
animate.default <- function(plot, ...) {
  stop('animation of ', class(plot)[1], ' objects not supported')
}
#' @rdname animate
#' @export
animate.gganim <- function(plot, nframes, fps, duration, detail, renderer, device, ref_frame, start_pause, end_pause, rewind, ...) {
  args <- prepare_args(
    nframes = nframes,
    fps = fps,
    duration = duration,
    detail = detail,
    renderer = renderer,
    device = device,
    ref_frame = ref_frame,
    start_pause = start_pause,
    end_pause = end_pause,
    rewind = rewind,
    ...
  )
  if (is_knitting() && identical(def_ren$renderer, args$renderer) && !def_ren$has_proper) {
    warning('No renderer available. Please install the gifski, av, or magick package to create animated output', call. = FALSE)
    return(invisible(NULL))
  }
  orig_nframes <- args$nframes
  args$nframes <- args$nframes - args$start_pause - args$end_pause
  if (args$rewind) {
    args$nframes <- ceiling((args$nframes - args$start_pause) / 2)
    args$end_pause <- ceiling(args$end_pause / 2)
  }
  nframes_total <- (args$nframes - 1) * args$detail + 1
  plot <- prerender(plot, nframes_total)
  nframes_final <- get_nframes(plot)

  frame_ind <- unique(round(seq(1, nframes_final, length.out = args$nframes)))

  if (args$device == 'current') {
    frame_ind <- c(rep(frame_ind[1], args$start_pause), frame_ind, rep(frame_ind[length(frame_ind)], args$end_pause))
    if (args$rewind) frame_ind <- c(frame_ind, rev(frame_ind))
    if (args$ref_frame < 0) {
      args$ref_frame <- args$ref_frame - args$end_pause
    } else {
      args$ref_frame <- args$ref_frame + args$start_pause
    }
  }

  if (args$nframes != length(frame_ind)) {
    message('nframes and fps adjusted to match transition')
    args$fps <- args$fps * length(frame_ind) / args$nframes
  }

  if (args$ref_frame < 0) args$ref_frame <- nframes_final + 1 + args$ref_frame

  frames_vars <- do.call(
    draw_frames,
    c(list(plot = plot,
           frames = frame_ind,
           device = args$device,
           ref_frame = args$ref_frame),
      args$dev_args)
  )
  if (args$device == 'current') return(invisible(frames_vars))

  if (args$start_pause != 0) frames_vars <- rbind(frames_vars[rep(1, args$start_pause), , drop = FALSE], frames_vars)
  if (args$end_pause != 0) frames_vars <- rbind(frames_vars, frames_vars[rep(nrow(frames_vars), args$end_pause), , drop = FALSE])
  if (args$rewind) frames_vars <- rbind(frames_vars, frames_vars[rev(seq_len(orig_nframes - nrow(frames_vars))), , drop = FALSE])

  animation <- args$renderer(frames_vars$frame_source, args$fps)
  attr(animation, 'frame_vars') <- frames_vars
  set_last_animation(animation)
  animation
}

#' Catch attempt to use the old API
#'
#' @export
#' @keywords internal
gganimate <- function(...) {
  stop(
    'It appears that you are trying to use the old API, which has been deprecated.\n',
    'Please update your code to the new API or install the old version of gganimate\n',
    'from https://github.com/thomasp85/gganimate/releases/tag/v0.1.1',
    call. = FALSE
  )
}
#' @rdname gganimate
#' @export
gg_animate <- gganimate

#' @importFrom utils modifyList
prepare_args <- function(nframes, fps, duration, detail, renderer, device, ref_frame, start_pause, end_pause, rewind, ...) {
  args <- list()
  chunk_args <- if (is_knitting()) get_knitr_options(knitr::opts_chunk$get(), unlist = FALSE) else list(dev_args = list())
  args$nframes <- nframes %?% chunk_args$nframes %||% getOption('gganimate.nframes', 100)
  args$fps <- fps %?% chunk_args$fps %||% getOption('gganimate.fps', 10)
  duration <- duration %?% chunk_args$duration %||% getOption('gganimate.duration', NULL)
  if (!is.null(duration)) {
    if (
      !missing(fps) ||
      is.null(args$nframes) ||
      (!is.null(getOption('gganimate.fps')) && is.null(getOption('gganimate.nframes')))
    ) args$nframes <- duration * args$fps
    else args$fps <- args$nframes / duration
  }
  if (is.null(args$nframes) || is.null(args$fps)) {
    stop("At least 2 of 'nframes', 'fps', and 'duration' must be given", call. = FALSE)
  }
  args$detail <- detail %?% chunk_args$detail %||% getOption('gganimate.detail', 1)
  args$renderer <- renderer %?% chunk_args$renderer %||% getOption('gganimate.renderer', def_ren$renderer)
  args$device <- tolower(device %?% chunk_args$device %||% getOption('gganimate.device', 'png'))
  if (args$device == 'svglite' && !requireNamespace('svglite', quietly = TRUE)) {
    stop('The svglite package is required to use this device', call. = FALSE)
  }
  if (args$device == 'ragg_png' && !requireNamespace('ragg', quietly = TRUE)) {
    stop('The ragg package is required to use this device', call. = FALSE)
  }
  args$ref_frame <- ref_frame %?% chunk_args$ref_frame %||% getOption('gganimate.ref_frame', 1)
  args$start_pause <- start_pause %?% chunk_args$start_pause %||% getOption('gganimate.start_pause', 0)
  args$end_pause <- end_pause %?% chunk_args$end_pause %||% getOption('gganimate.end_pause', 0)
  args$rewind <- rewind %?% chunk_args$rewind %||% getOption('gganimate.rewind', FALSE)
  dev_args <- list(...)
  args$dev_args <- if (length(dev_args) > 0) {
    modifyList(getOption('gganimate.dev_args', list()), dev_args)
  } else {
    modifyList(getOption('gganimate.dev_args', list()), chunk_args$dev_args)
  }
  args
}
# Build plot for a specific number of frames
prerender <- function(plot, nframes) {
  plot <- set_nframes(plot, nframes)
  ggplot_build(plot)
}
# Draw each frame as an image based on a specified device
# Returns a data.frame of frame metadata with image location in frame_source
# column
#' @importFrom future.apply future_mapply
draw_frames <- function(plot, frames, device, ref_frame, ...) {
  stream <- device == 'current'

  dims <- tryCatch(
    plot_dims(plot, ref_frame),
    error = function(e) {
      warning('Cannot get dimensions of plot table. Plot region might not be fixed', call. = FALSE)
      list(widths = NULL, heights = NULL)
    }
  )

  dir <- tempfile(pattern = '')
  dir.create(dir, showWarnings = FALSE)
  files <- file.path(dir, sprintf('gganim_plot%04d', seq_along(frames)))
  files <- switch(
    tolower(device),
    ragg_png = ,
    png = paste0(files, '.png'),
    jpg = ,
    jpeg = paste0(files, '.jpg'),
    tif = ,
    tiff = paste0(files, '.tif'),
    bmp = paste0(files, '.bmp'),
    svglite = ,
    svg = paste0(files, '.svg'),
    current = files,
    stop('Unsupported device', call. = FALSE)
  )
  device <- switch(
    device,
    ragg_png = ragg::agg_png,
    png = png,
    jpg = ,
    jpeg = jpeg,
    tif = ,
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

  void <- future_mapply(frames, files, seq_along(frames), FUN = function(frame, file, i, stream, ..., plot, dims, pb = NULL) {
    if (!stream) {
      device(file, ...)
      on.exit(dev.off())
    }

    tryCatch(
      plot$scene$plot_frame(plot, frame, widths = dims$widths, heights = dims$heights),
      error = function(e) {
        warning(conditionMessage(e), call. = FALSE)
      }
    )

    if (!is.null(pb)) {
      rate <- i/as.double(Sys.time() - start, units = 'secs')
      if (is.nan(rate)) rate <- 0
      rate <- format(rate, digits = 2)
      pb$tick(tokens = list(fps = rate))
    }
  },
  MoreArgs = list(stream = stream, ..., plot = plot, dims = dims, pb = pb),
  SIMPLIFY = FALSE, USE.NAMES = FALSE)

  frame_vars <- plot$scene$frame_vars[frames, , drop = FALSE]
  if (!stream) frame_vars$frame_source <- files
  frame_vars
}
# Get dimensions of plot based on a reference frame
plot_dims <- function(plot, ref_frame) {
  tmpf <- tempfile()
  png(tmpf)
  on.exit({
    dev.off()
    unlink(tmpf)
  })
  frame <- plot$scene$get_frame(plot, ref_frame)
  frame <- ggplot_gtable(frame)
  widths_rel <- frame$widths
  widths <- convertWidth(widths_rel, 'mm')
  heights_rel <- frame$heights
  heights <- convertHeight(heights_rel, 'mm')
  if (is.list(widths)) { # New unit spec
    null_widths <- vapply(unclass(widths), `[[`, numeric(1), 1L) == 0
    null_heights <- vapply(unclass(heights), `[[`, numeric(1), 1L) == 0
  } else {
    null_widths <- as.numeric(widths) == 0
    null_heights <- as.numeric(heights) == 0
  }
  widths[null_widths] <- widths_rel[null_widths]
  heights[null_heights] <- heights_rel[null_heights]
  list(widths = widths, heights = heights)
}

is_knitting <- function() isTRUE(getOption("knitr.in.progress"))
