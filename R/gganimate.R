#' Show an animation of a ggplot2 object
#'
#' Show an animation of a ggplot2 object that contains a \code{frame} aesthetic. This
#' \code{frame} aesthetic will determine which frame the animation is shown in. For
#' example, you could add the aesthetic \code{frame = time} to a dataset including
#' a \code{time} variable. Each distinct value of the frame aesthetic is rendered
#' into one frame of the resulting animation, in sorted order.
#'
#' If \code{cumulative = TRUE} is set within a layer along with a \code{frame} aesthetic,
#' the frames build cumulatively rather than each being generated with separate data.
#'
#' @param p A ggplot2 object. If no plot is provided, use the last plot by default.
#' @param filename Optionally, an output file to save to. If not given, will
#' store as plots without (yet) saving to a file
#' @param saver A string such as "mp4" or "gif" that specifies
#' a function from the animation package such as \code{saveVideo}
#' or \code{saveGIF} to use for saving. This can also be recognized from the
#' filename extension.
#' @param title_frame Whether to title each image with the current \code{frame} value.
#' The value is appended on to any existing title.
#' @param ... If saving to a file, extra arguments to pass along to the animation
#' saving function (to \code{saveVideo}/\code{saveGIF}/etc).
#'
#' @import ggplot2
#'
#' @examples
#'
#' library(ggplot2)
#' library(gapminder)
#'
#' p <- ggplot(gapminder, aes(gdpPercap, lifeExp, size = pop, color = continent, frame = year)) +
#'   geom_point() +
#'   scale_x_log10()
#'
#' p
#'
#' gganimate(p)
#'
#' \dontrun{
#' gganimate(p, "output.gif")
#' gganimate(p, "output.mp4")
#' }
#'
#' # You can also create cumulative graphs by adding the `cumulative = TRUE` aesthetic.
#' # For example, we could show the progression of temperature over time.
#'
#' aq <- airquality
#' aq$date <- as.Date(paste(1973, aq$Month, aq$Day, sep = "-"))
#'
#' p2 <- ggplot(aq, aes(date, Temp, frame = Month, cumulative = TRUE)) +
#'   geom_line()
#'
#' gganimate(p2, title_frame = FALSE)
#'
#'
#' @export
gganimate <- function(p = last_plot(), filename = NULL,
                       saver = NULL, title_frame = TRUE, ...) {
  if (is.null(p)) {
    stop("no plot to animate")
  }

  built <- ggplot_build(p)

  # get frames
  frames <- plyr::compact(lapply(built$data, function(d) d$frame))

  if (length(frames) == 0) {
    stop("No frame aesthetic found; cannot create animation")
  }

  if (is.factor(frames[[1]])) {
    # for factors, have to use unlist to combine
    frames <- sort(unique(unlist(frames)))
  } else {
    frames <- sort(unique(do.call(c, frames)))
  }
  frames <- sort(unique(frames))

  plots <- lapply(frames, function(f) {
    # replace each data object with a subset
    b <- built
    for (i in seq_along(b$data)) {
      frame_vec <- b$data[[i]]$frame
      if (!is.null(frame_vec)) {
        sub <- (frame_vec == f | is.na(frame_vec))
        if (!is.null(b$data[[i]]$cumulative)) {
          sub <- sub | (b$data[[i]]$cumulative & (frame_vec <= f))
        }

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

  ret <- list(plots = plots, frames = frames)
  class(ret) <- "gganimate"

  if (!is.null(filename)) {
    ret <- gganimate_save(ret, filename, saver, ...)
  } else {
    ret$ani_opts <- list(...)
    ret$saved <- FALSE
  }

  ret
}


#' Print a gganimate object, allowing browsing in RStudio
#'
#' Print a gganimate object as browsable HTML, which allows visualization
#' directly in RStudio. If we are in knitr, directly print each of the
#' images instead (you should use the \code{fig.show = "animate"} option
#' in the chunk).
#'
#' @param x gganimate object
#' @param format What format to display in, such as "gif" (default),
#' "mp4", or "avi".
#' @param ... Extra arguments for the <img> or <video> tag, such
#' as width or height
#'
#' This saves the plot to a file using \code{\link{gganimate_save}}
#' (and then loads the contents of that file into memory) if it has
#' not already been saved.
#'
#' @export
print.gganimate <- function(x, format = "gif", ...) {
  # if knitr is running, use a special case. Print all figures
  if (!(is.null(getOption("knitr.in.progress")))) {
    # don't print if it has already been saved
    if (!x$saved) {
      for (pl in x$plots) {
        plot_ggplot_build(pl)
      }
    }
    return()
  }

  # if it has not yet been saved to a file, save now (to a temporary file)
  if (!x$saved) {
    x <- do.call(gganimate_save, c(list(x, saver = format), x$ani_opts))
  }

  # construct HTML
  if (!is.null(x$mime_type) && grepl("^video", x$mime_type)) {
    d <- htmltools::tags$video(htmltools::tags$source(src = x$src),
                               autoplay = TRUE,
                               loop = TRUE, ...)
  } else if (!is.null(x$mime_type) && grepl("^image", x$mime_type)) {
    d <- htmltools::tags$img(src = x$src, ...)
  } else {
    message("opening gganimate file stored at ", x$filename)
    auto_browse(x$filename)
    return()
  }

  print(htmltools::browsable(d))
}
