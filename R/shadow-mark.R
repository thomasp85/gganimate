#' Show original data as background marks
#'
#' This shadow lets you show the raw data behind the current frame. Both past
#' and/or future raw data can be shown and styled as you want.
#'
#' @param past Should raw data from earlier in the animation be shown
#' @param future Should raw data from later in the animation be shown
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
#' # Use any of the aesthetics to add a style to either the past or the future raw data.
#' # Adding a grouping variable in a transition call prior to calling `shadow_mark()` will
#' # allow transitioning through different states in time.
#'
#' p1 <- ggplot(airquality, aes(Day, Temp)) +
#'   geom_line(color = 'red', size = 1) +
#'   transition_time(Month) +
#'   shadow_mark(colour = 'black', size = 0.75)
#'
#' # animate(p1)
#'
#' # Add a future = TRUE argument to show data later in the animation.
#'
#' p2 <- ggplot(airquality, aes(Day, Temp)) +
#'   geom_line(color = 'red', size = 1) +
#'   transition_time(Month) +
#'   shadow_mark(color = 'black', size = 0.75, past = FALSE, future = TRUE)
#'
#' # animate(p2)
#'
shadow_mark <- function(past = TRUE, future = FALSE, ..., exclude_layer = NULL) {
  dots <- quos(...)
  names(dots) <- sub('color', 'colour', names(dots))
  ggproto(NULL, ShadowMark,
    exclude_layer = exclude_layer,
    params = list(
      past = past,
      future = future,
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
ShadowMark <- ggproto('ShadowMark', Shadow,
  train = function(self, data, params) {
    params$raw <- lapply(seq_along(data), function(i) {
      d <- data[[i]]
      if (i %in% params$excluded_layers) {
        return(d[[1]][0, , drop = FALSE])
      }
      d <- lapply(d, function(dd) {
        dd[dd$.phase == 'raw', , drop = FALSE]
      })
      frames <- rep(seq_along(d), vapply(d, nrow, integer(1)))
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
      if (e || (!params$past && !params$future)) return(d[[1]])
      s <- if (params$past && params$future) {
        s[s$.frame != frame_ind, , drop = FALSE]
      } else if (params$past) {
        s[s$.frame < frame_ind, , drop = FALSE]
      } else if (params$future) {
        s[s$.frame > frame_ind, , drop = FALSE]
      }
      if (nrow(s) == 0) return(d[[1]])
      s$.frame <- NULL
      s$group <- s$group - (max(s$group) + 1) # make sure shadow groups are prior to frame group
      vec_rbind0(s, d[[1]])
    }, d = data, s = params$raw, e = seq_along(data) %in% params$excluded_layers)
  }
)
