#' Save an animation to a file
#'
#' This function is analogous to [ggplot2::ggsave()] in that it by default takes
#' the last created animation and saves it to the specific location. As
#' `gganimate` supports arbitrary renderers, and thus return types, the returned
#' object must implement a `save_animation` method to be able to be used with
#' `anim_save()`. This is provided natively for `gif_image` and `magick-image`
#' objects.
#'
#' @param animation The animation object to save, as returned by [animate()].
#' Defaults to the last rendered animation using [last_animation()]
#' @inheritParams ggplot2::ggsave
#'
#' @export
#'
anim_save <- function(filename, animation = last_animation(), path = NULL) {
  if (!is.null(path)) {
    filename <- file.path(path, filename)
  }
  save_animation(animation, filename)
}
#' Infrastructure for animation saving
#'
#' Any returned animation object that wish to support [anim_save()] need to
#' implement this method
#'
#' @param animation The animation object to save
#' @param file The file path to save it to
#'
#' @keywords internal
#' @export
save_animation <- function(animation, file) {
  UseMethod('save_animation')
}
#' @export
save_animation.default <- function(animation, file) {
  stop('The animation object does not specify a save_animation method', call. = FALSE)
}
#' @export
save_animation.gif_image <- function(animation, file) {
  file.copy(animation, file, overwrite = TRUE)
  invisible(NULL)
}
#' @export
save_animation.video_file <- save_animation.gif_image
#' @export
`save_animation.magick-image` <- function(animation, file) {
  if (!requireNamespace('magick', quietly = TRUE)) {
    stop('The magick package is required for this functionality', call. = FALSE)
  }
  magick::image_write(animation, file)
  invisible(NULL)
}
