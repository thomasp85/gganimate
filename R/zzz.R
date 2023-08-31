def_ren <- new.env(parent = emptyenv())

.onLoad <- function(...) {
  register_s3_method("knitr", "knit_print", "gganim")
  register_s3_method("knitr", "knit_print", "gif_image")
  register_s3_method("knitr", "knit_print", "video_file")
  register_s3_method("knitr", "knit_print", "sprite_image")

  def_ren$has_proper <- TRUE
  if (requireNamespace('gifski', quietly = TRUE)) {
    def_ren$renderer <- gifski_renderer()
  } else if (requireNamespace('magick', quietly = TRUE)) {
    def_ren$renderer <- magick_renderer()
  } else if (requireNamespace('av', quietly = TRUE)) {
    def_ren$renderer <- av_renderer()
  } else {
    def_ren$renderer <- file_renderer()
    def_ren$has_proper <- FALSE
  }

  invisible()
}
.onAttach <- function(...) {
  if (!isTRUE(def_ren$has_proper)) {
    packageStartupMessage(
      'No renderer backend detected. gganimate will default to writing frames to separate files\n',
      'Consider installing:\n',
      '- the `gifski` package for gif output\n',
      '- the `av` package for video output\n',
      'and restarting the R session'
    )
  }
}

register_s3_method <- function(pkg, generic, class, fun = NULL) {
  check_string(pkg, allow_empty = FALSE)
  check_string(generic, allow_empty = FALSE)
  check_string(class, allow_empty = FALSE)

  if (is.null(fun)) {
    fun <- get(paste0(generic, ".", class), envir = parent.frame())
  } else {
    check_function(fun)
  }

  if (pkg %in% loadedNamespaces()) {
    registerS3method(generic, class, fun, envir = asNamespace(pkg))
  }

  # Always register hook in case package is later unloaded & reloaded
  setHook(
    packageEvent(pkg, "onLoad"),
    function(...) {
      registerS3method(generic, class, fun, envir = asNamespace(pkg))
    }
  )
}
