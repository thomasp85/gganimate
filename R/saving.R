#' Save a gganimate object to a file
#'
#' @param g A gganimate object
#' @param filename File to write to
#' @param saver A string such as "mp4" or "gif" that specifies
#' a function from the animation package such as \code{saveVideo}
#' to use for saving. GIFs are saved manually using ImageMagick.
#' @param ... Additional arguments passed on to the saving function,
#' such as \code[pkg=ggplot2]{ggsave} for GIFs or
#' \code[pkg=animate]{saveVideo} for MP4.
#'
#' @details If saving to a GIF, uses a custom method that takes advantage
#' of redundant backgrounds (scales, static layers, etc).
#'
#' @export
gganimate_save <- function(g, filename = NULL, saver = NULL,
                           fps = 1, loop = 0, ...) {
  # save to a temporary file if necessary
  if (is.null(filename)) {
    if (is.null(saver)) {
      filename <- gganimate_tempfile(fileext = ".gif")
    } else {
      filename <- gganimate_tempfile(fileext = paste0(".", saver))
    }
  }

  # figure out how it should be saved
  s <- animation_saver(saver, filename)

  # temporarily move to directory (may be current one, that's OK)
  # this helps with animation functions like saveGIF that work only in
  # current directory
  withr::with_dir(dirname(filename), {
    if (s$saver == "gif" && FALSE) {
      save_gganimate_custom(g, filename = filename, ...)
    } else {
      s$func(for (pl in g$plots) {
        plot_ggplot_build(pl)
      }, basename(filename), autobrowse = FALSE, ...)
    }
  })

  g$filename <- filename
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
                     webm = "video/webm",
                     avi = "video/avi")

  list(saver = saver, func = savers[[saver]], mime_type = mime_types[[saver]])
}

# utility:
save_gganimate_custom <- function(g, filename, clean = TRUE, ...) {
  blank <- g$plots[[1]]
  blank$data <- lapply(blank$data, function(d) utils::head(d, 0))
  blank$plot$labels$title <- " "
  blank_gtable <- ggplot2::ggplot_gtable(blank)

  # align all of the plots
  gtables <- lapply(g$plots, function(p) {
    p$plot$theme <- theme_void()
    ggplot2::ggplot_gtable(p)
  })
  gtables_aligned <- cowplot::align_plots(
    plotlist = c(list(blank_gtable), gtables),
    align = "hv")

  filenames <- paste0("plot", seq_along(gtables_aligned), ".png")
  for (i in seq_along(gtables_aligned)) {
    bg <- ifelse(i == 1, "white", "transparent")
    suppressMessages(ggsave(gtables_aligned[[i]],
                            filename = filenames[i],
                            bg = bg, ...))
  }

  command <- paste("convert -dispose none -delay 0 %s",
                   "-dispose previous -delay %d %s",
                   "-loop 0 %s")

  opts <- "-dispose none -delay 0 plot1.png -dispose previous"
  animation::im.convert(filenames[-1], basename(filename), extra.opts = opts,
                        clean = clean)
  unlink(filenames[1])
}

