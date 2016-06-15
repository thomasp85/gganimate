#' Save a gganimate object to a file
#'
#' @param g A gg_animate object
#' @param filename File to write to
#' @param saver A string such as "mp4" or "gif" that specifies
#' a function from the animation package such as \code{saveVideo}
#' or \code{saveGIF} to use for saving. This can also be recognized from the
#' filename extension.
#' @param ... Additional arguments passed on to the saving function,
#' such as \code{saveVideo} or \code{saveGIF} from the animation
#' package.
#'
#' @export
gg_animate_save <- function(g, filename = NULL, saver = NULL, ...) {
  # save to a temporary file if necessary
  if (is.null(filename)) {
    if (is.null(saver)) {
      filename <- gganimate_tempfile(fileext = ".gif")
    } else {
      filename <- gganimate_tempfile(fileext = paste0(".", saver))
    }
  }

  g$filename <- filename

  # figure out how it should be saved
  s <- animation_saver(saver, filename)

  # temporarily move to directory (may be current one, that's OK)
  # this helps with animation functions like saveGIF that work only in
  # current directory
  withr::with_dir(dirname(filename), {
    s$func(for (pl in g$plots) {
      plot_ggplot_build(pl)
    }, basename(filename), autobrowse = FALSE, ...)
  })

  if (!is.null(s$mime_type)) {
    # if it can be displayed in R, import it as an encoded string
    g$src <- base64enc::dataURI(file = filename, mime = s$mime_type)
    g$mime_type <- s$mime_type
  }
  g$saved <- TRUE
  g
}


#' Retrieve a function for saving animations based on a string/function or a filename
#'
#' @param saver A function or string describing an animation saver
#' @param filename File name to save to
#' @param mime_type If saver is a custom function instead of a string
#' specification, can specify a mime_type to save it as. Without this,
#' files can be saved but not browsed in RStudio.
animation_saver <- function(saver, filename, mime_type = NULL) {
  if (is.function(saver)) {
    return(list(func = saver, mime_type = mime_type))
  }
  if (is.null(saver)) {
    saver <- tolower(tools::file_ext(filename))
  }
  savers <- list(gif = animation::saveGIF,
                 mp4 = animation::saveVideo,
                 webm = animation::saveVideo,
                 avi = animation::saveVideo,
                 mpeg = function(expr, filename, ...) {
                   animation::saveVideo(expr, filename,
                                        other.opts = ifelse(animation::ani.options('interval') >= 1/24, "-r 24", "-r 30")) },
                 html = function(expr, filename, ...) animation::saveHTML(expr, htmlfile = filename, ...),
                 tex = function(expr, filename, ...) animation::saveLatex(expr, latex.filename = filename, ...),
                 pdf = function(expr, filename, ...) animation::saveLatex(expr, latex.filename = gsub("pdf$", "tex", filename, perl = TRUE)),
                 swf = animation::saveSWF)

  if (is.null(savers[[saver]])) {
    stop("Don't know how to save animation of type ", saver)
  }

  # for those that can be viewed in RStudio, save the mime_type
  mime_types <- list(gif = "image/gif",
                     mp4 = "video/mp4",
                     mpeg = "video/mpeg",
                     webm = "video/webm",
                     avi = "video/avi")

  list(saver = saver, func = savers[[saver]], mime_type = mime_types[[saver]])
}
