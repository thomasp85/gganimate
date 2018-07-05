#' @include view-step.R
NULL

#' Pan and zoom smoothly between different states
#'
#' This view is in many ways equivalent to [view_step()] and
#' [view_step_manual()]but instead of simply tweening the bounding box of each
#' view it implement the smooth zoom and pan technique developed by Reach &
#' North (2018). It gradually zooms out and then in during the pan to allow a
#' smooth transition of the view.
#'
#' @references Reach, A., North, C. (2018) *Smooth, Efficient, and Interruptible Zooming and Panning*. IEEE Transactions on Visualization and Computer Graphics DOI:10.1109/TVCG.2018.2800013
#'
#' @inheritParams view_step
#'
#' @family views
#'
#' @export
#' @importFrom ggplot2 ggproto
view_zoom <- function(pause_length, step_length, nsteps = NULL, look_ahead = pause_length,
                      delay = 0, include = FALSE, wrap = TRUE, pause_first = FALSE,
                      fixed_x = FALSE, fixed_y = FALSE, exclude_layer = NULL, aspect_ratio = 1) {
  ggproto(NULL, ViewZoom,
          fixed_lim = list(x = fixed_x, y = fixed_y),
          exclude_layer = exclude_layer,
          aspect_ratio = aspect_ratio,
          params = list(
            pause_length = pause_length,
            step_length = step_length,
            nsteps = nsteps,
            look_ahead = look_ahead,
            delay = delay,
            include = include,
            wrap = wrap,
            pause_first = pause_first
          )
  )
}

transition_window <- function(windows, next_window, n, params) {
  if (is.null(windows$.frame)) {
    windows$.id <- seq_len(nrow(windows))
    windows$.phase <- 'raw'
    windows$.frame <- seq_len(nrow(windows))
  } else {
    attr(windows, 'nframes') <- NULL
  }
  if (nrow(windows) == 1) n <- n - 1
  start <- windows[nrow(windows), , drop = FALSE]
  w0 <- start$xmax - start$xmin
  w1 <- next_window$xmax - next_window$xmin
  x0 <- start$xmin + w0/2
  x1 <- next_window$xmin+ w1/2
  x_traj <- trajectory(x0, x1, w0, w1, n + 1)

  h0 <- start$ymax - start$ymin
  h1 <- next_window$ymax - next_window$ymin
  y0 <- start$ymin + h0/2
  y1 <- next_window$ymin+ h1/2
  y_traj <- trajectory(y0, y1, h0, h1, n + 1)

  new_windows <- data.frame(
    xmin = as.vector(x_traj$u - x_traj$v/2),
    xmax = as.vector(x_traj$u + x_traj$v/2),
    ymin = as.vector(y_traj$u - y_traj$v/2),
    ymax = as.vector(y_traj$u + y_traj$v/2),
    .frame = seq_len(n + 1) - 1 + if (is.null(start$.frame)) 0 else start$.frame
  )
  missing_cols <- setdiff(names(start), names(new_windows))
  new_windows[missing_cols] <- start[1, missing_cols, drop = FALSE]
  rbind(
    windows,
    new_windows[-1, match(names(start), names(new_windows))]
  )
}

#' @rdname gganimate-ggproto
#' @format NULL
#' @usage NULL
#' @export
#' @importFrom ggplot2 ggproto
ViewZoom <- ggproto('ViewZoom', ViewStep,
                    window_transition = transition_window
)

trajectory <- function(u0, u1, v0, v1, n) {
  if (all(u0 == u1)) {
    u <- matrix(u0, ncol = length(u0), nrow = n, byrow = TRUE)
    k <- sign(v1 - v0)
    S <- abs(log(v1/v0))
    s <- seq(0, S, length.out = n)
    v <- v0 * exp(s * k)
  } else {
    lu <- sqrt(sum((u1 - u0)^2))
    r0 <- ri(v1, v0, 0, lu)
    r1 <- ri(v1, v0, 1, lu)
    S <- r1 - r0
    s <- seq(0, S, length.out = n)
    cosh_s_r0 <- cosh(s + r0)
    v <- v0 * cosh(r0) / cosh_s_r0
    u_part1 <- v0 * (sinh(s) / cosh_s_r0)
    u_part2 <- (u1 - u0) / lu
    u_part12 <- vapply(u_part2, `*`, u_part1, u_part1)
    u <- matrix(u0, ncol = length(u0))[rep(1, length(u_part1))] + u_part12
  }
  list(u = u, v = v)
}

ri <- function(v1, v0, i, lu) {
  top <- v1^2 - v0^2 + (-1)^i * lu^2
  bottom <- -2 * (if (i == 0) v0 else v1) * lu
  asinh(top/bottom)
}
