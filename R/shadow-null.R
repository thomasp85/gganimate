#' A non-existent shadow
#'
#' This is the default shadow that simply doesn't show anything other than the
#' data for the current frame.
#'
#' @family shadows
#'
#' @export
shadow_null <- function() {
  ggproto(NULL, ShadowNull)
}

#' @rdname gganimate-ggproto
#' @format NULL
#' @usage NULL
#' @export
ShadowNull <- ggproto('ShadowNull', Shadow,
  prepare_shadow = function(self, shadow, params) {
    shadow
  },
  prepare_frame_data = function(self, data, shadow, params, frame_ind, shadow_ind) {
    lapply(data, `[[`, 1)
  }
)
