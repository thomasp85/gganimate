#' A trail of evenly spaced old frames
#'
#' This shadow will trace the movement in your animation by keeping every *n*th
#' frame and will thus produce a breadcrumb-like trail. Note that the shadow
#' frames will not be equidistant in space but in time (that is, if a point
#' moves slowly the *crumbs* will be closer to each other). It is possible to
#' modify the look of the shadow by changing the different graphic parameters in
#' the data
#'
#' @param distance The temporal distance between the frames to show, as a
#' fraction of the full animation length
#' @param max_frames The maximum number of shadow frames to show
#' @param ... changes to the shadow data, e.g. `alpha = alpha/2` or
#' `colour = 'grey'`
#' @param exclude_layer Indexes of layers that should be excluded.
#'
#' @family shadows
#'
#' @importFrom ggplot2 ggproto
#' @importFrom rlang quos
#' @export
#'
#' @examples
#' anim <- ggplot(airquality, aes(Day, Temp, colour = factor(Month))) +
#'   geom_point() +
#'   transition_time(Day)
#'
#' # Change distance between points
#' anim1 <- anim +
#'   shadow_trail(0.02)
#'
#' # Style shadow differently
#' anim2 <- anim +
#'   shadow_trail(alpha = 0.3, shape = 2)
#'
#' # Restrict the shadow to 10 frames
#' anim3 <- anim +
#'   shadow_trail(max_frames = 10)
#'
shadow_trail <- function(distance = 0.05, max_frames = Inf, ..., exclude_layer = NULL) {
  dots <- quos(...)
  names(dots) <- sub('color', 'colour', names(dots))
  ggproto(NULL, ShadowTrail,
          exclude_layer = exclude_layer,
          params = list(
            distance = distance,
            max_frames = max_frames,
            dots = dots
          )
  )
}
#' @rdname gganimate-ggproto
#' @format NULL
#' @usage NULL
#' @export
#' @importFrom ggplot2 ggproto
#' @importFrom rlang eval_tidy
ShadowTrail <- ggproto('ShadowTrail', Shadow,
  train = function(self, data, params) {
    params$distance <- round(params$nframes * params$distance)
    params$shadow_frames <- seq(1, params$nframes, by = params$distance)
    params$raw <- lapply(seq_along(data), function(i) {
      d <- data[[i]]
      if (i %in% params$excluded_layers) {
        return(d[[1]][0, , drop = FALSE])
      }
      d <- d[params$shadow_frames]
      frames <- rep(params$shadow_frames, vapply(d, nrow, integer(1)))
      d <- vec_rbind0(!!!d)
      for (i in names(params$dots)) {
        if (!is.null(d[[i]])) d[[i]] <- eval_tidy(params$dots[[i]], d)
      }
      cbind(d, .frame = frames)
    })
    params
  },
  prepare_frame_data = function(self, data, shadow, params, frame_ind, shadow_ind) {
    Map(function(d, s, e) {
      if (e) return(d[[1]])
      s <- s[s$.frame < frame_ind, , drop = FALSE]
      frames <- unique0(s$.frame)
      if (params$max_frames < length(frames)) {
        first_frame <- sort(frames, decreasing = TRUE)[params$max_frames]
        s <- s[s$.frame >= first_frame, , drop = FALSE]
      }
      s$.frame <- NULL
      vec_rbind0(s, d[[1]])
    }, d = data, s = params$raw, e = seq_along(data) %in% params$excluded_layers)
  }
)
