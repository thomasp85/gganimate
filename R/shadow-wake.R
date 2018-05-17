#' Show preceeding frames with gradual falloff
#'
#' This shadow is meant to draw a small wake after data by showing the latest
#' frames up to the current. You can choose to gradually diminish the size
#' and/or opacity of the shadow. The length of the wake is not given in absolute
#' frames as that would make the animation susceptible to changes in the
#' framerate. Instead it is given as a proportion of the total length of the
#' animation.
#'
#' @param wake_length A number between 0 and 1 giving the length of the wake,
#' in relation to the total number of frames.
#' @param size Boolean indicating whether the size of the geom should shrink
#' @param alpha Boolean indicating whether geoms should get more translucent
#' @param falloff An easing function that control how size and/or alpha should
#' change.
#' @param wrap Should the shadow wrap around, so that the first frame will get
#' shadows from the end of the animation.
#' @param exclude_layer Indexes of layers that should be excluded.
#' @param exclude_phase Element phases that should not get a shadow. Possible
#' values are `'enter'`, `'exit'`, `'static'`, `'transition'`, and `'raw'`. If
#' `NULL` all phases will be included. Defaults to `'enter'` and `'exit'`
#'
#' @family shadows
#'
#' @export
#' @importFrom ggplot2 ggproto
shadow_wake <- function(wake_length, size = TRUE, alpha = TRUE, falloff = 'cubic-in', wrap = TRUE, exclude_layer = NULL, exclude_element = c('enter', 'exit')) {
  ggproto(NULL, ShadowWake,
    exclude_layer = exclude_layer,
    params = list(
      wake_length = wake_length,
      size = size,
      alpha = alpha,
      falloff = falloff,
      wrap = wrap,
      exclude_element = exclude_element
    )
  )
}
#' @rdname gganimate-ggproto
#' @format NULL
#' @usage NULL
#' @export
#' @importFrom ggplot2 ggproto
#' @importFrom tweenr tween_numeric
ShadowWake <- ggproto('ShadowWake', Shadow,
  setup_params = function(self, data, params) {
    params$wake_length <- round(params$nframes * params$wake_length)
    params$falloff <- tween_numeric(c(0, 1), params$wake_length + 2, params$falloff)[[1]][1 + seq_len(params$wake_length)]
    params
  },
  get_frames = function(self, params, i) {
    frames <- rev(i - seq_len(params$wake_length))
    if (params$wrap) {
      frames <- frames %% params$nframes
      frames[frames == 0] <- params$nframes
    } else {
      frames <- frames[frames > 0 && frames <= params$nframes]
    }
    frames
  },
  prepare_shadow = function(self, shadow, params) {
    lapply(shadow, function(d) {
      i <- rep(params$falloff, vapply(d, nrow, integer(1)))
      d <- do.call(rbind, d)
      if (!is.null(d$alpha)) d$alpha <- d$alpha * i
      if (!is.null(d$size)) d$size <- d$size * i
      if (!is.null(d$stroke)) d$stroke <- d$stroke * i
      d
    })
  },
  prepare_frame_data = function(self, data, shadow, params, frame_ind, shadow_ind) {
    Map(function(d, s, e) {
      if (e) return(d[[1]])
      ids <- d[[1]]$.id[!d[[1]]$.phase %in% params$exclude_element]
      s <- s[s$.id %in% ids, , drop = FALSE]
      rbind(s, d[[1]])
    }, d = data, s = shadow, e = seq_along(data) %in% self$exclude_layer)
  }
)
