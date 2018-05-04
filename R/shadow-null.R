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
  prepare_frame_data = function(self, data, shadow, params) {
    lapply(data, `[[`, 1)
  }
)
