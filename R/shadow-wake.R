#' Show preceding frames with gradual falloff
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
#' @param size Numeric indicating the size the wake should end on. If `NULL`
#' then size is not modified. Can also be a boolean with `TRUE` beeing equal `0`
#' and `FALSE` beeing equal to `NULL`
#' @param alpha as `size` but for alpha modification of the wake
#' @param colour,fill colour or fill the wake should end on. If `NULL` they are
#' not modified.
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
#' @importFrom ggplot2 ggproto
#' @export
#'
#' @examples
#' anim <- ggplot(iris, aes(Petal.Length, Sepal.Length)) +
#'   geom_point() +
#'   labs(title = "{closest_state}") +
#'   transition_states(Species, transition_length = 4, state_length = 1)
#'
#' # `shadow_wake` can be combined with e.g. `transition_states` to show
#' # motion of geoms as they are in transition with respect to the selected state.
#' anim1 <- anim +
#'   shadow_wake(wake_length = 0.05)
#'
#' # Different qualities can be manipulated by setting a value for it that it
#' # should taper off to
#' anim2 <- anim +
#'   shadow_wake(0.1, size = 10, alpha = FALSE, colour = 'grey92')
#'
#' # Use `detail` in the `animate()` call to increase the number of calculated
#' # frames and thus make the wake smoother
#' \dontrun{
#' animate(anim2, detail = 5)
#' }
#'
shadow_wake <- function(wake_length, size = TRUE, alpha = TRUE, colour = NULL, fill = NULL, falloff = 'cubic-in', wrap = TRUE, exclude_layer = NULL, exclude_phase = c('enter', 'exit')) {
  if (is_logical(size)) size <- if (size) 0 else NULL
  if (is_logical(alpha)) alpha <- if (alpha) 0 else NULL

  ggproto(NULL, ShadowWake,
    exclude_layer = exclude_layer,
    params = list(
      wake_length = wake_length,
      colour = colour,
      fill = fill,
      size = size,
      alpha = alpha,
      falloff = falloff,
      wrap = wrap,
      exclude_phase = exclude_phase
    )
  )
}
#' @rdname gganimate-ggproto
#' @format NULL
#' @usage NULL
#' @export
#' @importFrom ggplot2 ggproto
#' @importFrom tweenr tween_at
ShadowWake <- ggproto('ShadowWake', Shadow,
  setup_params = function(self, data, params) {
    params$wake_length <- round(params$nframes * params$wake_length)
    params$at <- seq(0, 1, length = params$wake_length + 1)[seq_len(params$wake_length)]
    params
  },
  get_frames = function(self, params, i) {
    frames <- rev(i - seq_len(params$wake_length))
    if (params$wrap) {
      frames <- frames %% params$nframes
      frames[frames == 0] <- params$nframes
    } else {
      frames <- frames[frames > 0 & frames <= params$nframes]
    }
    frames
  },
  prepare_shadow = function(self, shadow, params) {
    lapply(shadow, function(d) {
      if (length(d) == 0) return(NULL)
      i <- rep(params$at[seq_along(d)], vapply(d, nrow, integer(1)))
      d <- vec_rbind0(!!!d)

      if (!is.null(params$colour)) {
        if (!is.null(d$colour)) d$colour <- tween_at(params$colour, d$colour, i, params$falloff)
        if (!is.null(d$edge_colour)) d$edge_colour <- tween_at(params$colour, d$edge_colour, i, params$falloff)
      }
      if (!is.null(params$fill)) {
        if (!is.null(d$fill)) d$colour <- tween_at(params$fill, d$fill, i, params$falloff)
        if (!is.null(d$edge_fill)) d$edge_fill <- tween_at(params$fill, d$edge_fill, i, params$falloff)
      }
      if (!is.null(params$alpha)) {
        if (!is.null(d$edge_alpha)) {
          no_alpha <- is.na(d$edge_alpha)
          d$edge_alpha[!no_alpha] <- tween_at(params$alpha, d$edge_alpha[!no_alpha], i, params$falloff)
        } else if (!is.null(d$alpha)) {
          no_alpha <- is.na(d$alpha)
          d$alpha[!no_alpha] <- tween_at(params$alpha, d$alpha[!no_alpha], i, params$falloff)
        } else {
          no_alpha <- TRUE
        }
        if (!is.null(d$colour)) d$colour[no_alpha] <- mod_alpha(d$colour[no_alpha], i, params$alpha, params$falloff)
        if (!is.null(d$fill)) d$fill[no_alpha] <- mod_alpha(d$fill[no_alpha], i, params$alpha, params$falloff)
        if (!is.null(d$edge_colour)) d$edge_colour[no_alpha] <- mod_alpha(d$edge_colour[no_alpha], i, params$alpha, params$falloff)
        if (!is.null(d$edge_fill)) d$edge_fill[no_alpha] <- mod_alpha(d$edge_fill[no_alpha], i, params$alpha, params$falloff)
      }
      if (!is.null(params$size)) {
        if (!is.null(d$size)) d$size <- tween_at(params$size, d$size, i, params$falloff)
        if (!is.null(d$edge_size)) d$edge_size <- tween_at(params$size, d$edge_size, i, params$falloff)
        if (!is.null(d$edge_width)) d$edge_width <- tween_at(params$size, d$edge_width, i, params$falloff)
        if (!is.null(d$stroke)) d$stroke <- tween_at(params$size, d$stroke, i, params$falloff)
      }
      d
    })
  },
  prepare_frame_data = function(self, data, shadow, params, frame_ind, shadow_ind) {
    Map(function(d, s, e) {
      if (e) return(d[[1]])
      ids <- d[[1]]$.id[!d[[1]]$.phase %in% params$exclude_phase]
      s <- s[s$.id %in% ids, , drop = FALSE]
      d <- vec_rbind0(s, d[[1]])
      d[order(match(d$.id, unique0(d$.id))), , drop = FALSE]
    }, d = data, s = shadow, e = seq_along(data) %in% params$excluded_layers)
  }
)

#' @importFrom scales alpha
#' @importFrom grDevices col2rgb
mod_alpha <- function(col, i, end, ease) {
  alpha <- col2rgb(col, TRUE)[4,] / 255
  alpha(col, tween_at(end, alpha, i, ease))
}
