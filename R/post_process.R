#' Access metadata about the frames in an animation
#'
#' This function extracts the metadata generated about each frame during
#' rendering. It corresponds to the information available to the labels for
#' string literal interpretation and is thus dependent on the transition and
#' view used for the animation.
#'
#' @param animation The animation to extract metadata from. Defaults to the last
#' rendered animation
#'
#' @return A data.frame with a row per frame in the animation and metadata as
#' columns
#'
#' @export
frame_vars <- function(animation = last_animation()) {
  attr(animation, 'frame_vars')
}

#' Split an animation into chunks
#'
#' Sometimes it is of interest to split an animation out in smaller chunks so
#' they can be orchestrated, e.g. in a presentation. This function lets you
#' provide a 'factor' to split by in the same way as [base::split()] though this
#' one will be evaluated in the context of the animations [frame_vars()] data,
#' so you can split directly on frame metadata.
#'
#' @inheritParams frame_vars
#' @param by An unquoted expression to be evaluated in the context of the frame
#' metadata. The result must be of equal length to the number of frames in the
#' animation and define a grouping
#'
#' @return Depending on the output type of the renderer used to produce the
#' animation. Often a list with elements referencing the chunks of the
#' animation. that can then be saved individually.
#'
#' @export
split_animation <- function(animation = last_animation(), by) {
  by <- enquo(by)
  fv <- frame_vars(animation)
  by <- eval_tidy(by, fv)
  if (length(by) != nrow(fv)) {
    cli::cli_abort('{.arg by} must have the same length as the number of frames')
  }
  try_fetch(
    split(animation, by),
    error = function(e) {
      cli::cli_abort('The renderer output doesn\'t support splitting')
    }
  )
}

