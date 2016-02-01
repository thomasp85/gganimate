#' Show an animation of a ggplot2 object
#'
#' Show an animation of a ggplot2 object that contains a \code{frame} aesthetic. This
#' \code{frame} aesthetic will determine which frame the animation is shown in. For
#' example, you could add the aesthetic \code{frame = time} to a dataset including
#' a \code{time} variable.
#'
#' @param p A ggplot2 object. If no plot is provided, use the last plot by default.
#' @param filename Output file. If not given, simply prints to the screen (typical for animated
#' knitr chunks)
#' @param saver Can specify a function (or a string such as "mp4" or "html" that specifies
#' a function to use for saving
#' @param pause Amount of time to pause between displaying each plot. Only used when
#' displaying to the screen, not saving to a file (and not useful when creating an
#' animation in a knitr chunk). When saving to a file, use
#' \code{ani.options(interval = ...)}
#' @param title_frame Whether to title each image with the current \code{frame} value.
#' @param ... If saving to a file, extra arguments to pass along to the animation
#' saving function (to \code{saveVideo}/\code{saveGIF}/etc)
#'
#' @import ggplot2
#'
#' @export
gg_animate <- function(p = last_plot(), filename = NULL, saver = NULL,
                       pause = NULL, title_frame = TRUE, ...) {
  if (is.null(p)) {
    stop("no plot to save")
  }

  # add group mappings

  built <- ggplot_build(p)

  # get frames
  frames <- sort(unique(unlist(plyr::compact(lapply(built$data, function(d) d$frame)))))
  if (length(frames) == 0) {
    stop("No frame aesthetic found; cannot create animation")
  }

  plots <- lapply(frames, function(f) {
    # replace each data object with a subset
    b <- built
    for (i in seq_along(b$data)) {
      if (!is.null(b$data[[i]]$frame)) {
        sub <- b$data[[i]]$frame == f
        b$data[[i]] <- b$data[[i]][sub, ]
      }
    }

    # title plot according to frame
    if (title_frame) {
      if (!is.null(b$plot$labels$title)) {
        b$plot$labels$title <- paste(b$plot$labels$title, f)
      } else {
        b$plot$labels$title <- f
      }
    }

    b
  })

  if (!is.null(filename))  {
    saver_func <- animation_saver(saver, filename)

    saver_func(for (pl in plots) {
      plot_ggplot_build(pl)
    }, filename, ...)
  } else {
    for (pl in plots) {
      plot_ggplot_build(pl)
      if (!is.null(pause)) {
        Sys.sleep(pause)
      }
    }
  }
}


#' Retrieve a function for saving animations based on a string/function and a filename
#'
#' @param saver A function or string describing an animation saver
#' @param filename File name to save to
animation_saver <- function(saver, filename) {
  if (is.function(saver)) {
    return(saver)
  }
  if (is.null(saver)) {
    saver <- tolower(tools::file_ext(filename))
  }
  savers <- list(gif = animation::saveGIF,
                 mp4 = animation::saveVideo,
                 webm = animation::saveVideo,
                 avi = animation::saveVideo,
                 html = function(expr, filename, ...) animation::saveHTML(expr, htmlfile = filename, ...),
                 swf = animation::saveSWF)

  if(is.null(savers[[saver]])) {
    stop("Don't know how to save animation of type ", saver)
  }

  savers[[saver]]
}
