#' @rdname gganimate-ggproto
#' @format NULL
#' @usage NULL
#' @export
#' @importFrom ggplot2 ggproto
Transition <- ggproto('Transition', NULL,
  params = NULL,
  setup_params = function(self, data, params) {
    params
  },
  map_data = function(self, data, params) {
    data
  },
  expand_data = function(self, data, type, ease, enter, exit, params, layer_index) {
    data
  },
  unmap_frames = function(self, data, params) {
    data
  },
  remap_frames = function(self, data, params) {
    data
  },
  finish_data = function(self, data, params) {
    data
  },
  adjust_nframes = function(self, data, params, nframes) {
    nframes
  },
  get_frame_data = function(self, data, params, i) {
    lapply(data, `[`, i)
  },
  add_label_vars = function(self, var, i, params, plot) {
    var
  }
)
#' @importFrom ggplot2 ggplot_add
#' @export
ggplot_add.Transition <- function(object, plot, objectname) {
  plot <- as.gganim(plot)
  plot$transition <- object
  plot
}

# HELPERS -----------------------------------------------------------------


#' @importFrom tweenr tween_constant
get_frame_info <- function(static_levels, static_lengths, transition_lengths, nframes, static_first, static_name, ...) {
  if (static_first) {
    frames <- as.vector(rbind(static_lengths, transition_lengths))
    phase <- rep(rep(c('static', 'transition'), length(static_lengths)), frames)[seq_len(nframes)]
  } else {
    frames <- as.vector(rbind(transition_lengths, static_lengths))
    phase <- rep(rep(c('transition', 'static'), length(static_lengths)), frames)[seq_len(nframes)]
  }
  statics <- rep(static_levels, each = 2)
  if (static_first) {
    previous_static <- rep(statics, frames)[seq_len(nframes)]
    statics2 <- c(statics[-1], statics[1])
    next_static <- rep(statics2, frames)[seq_len(nframes)]
  } else {
    next_static <- rep(statics, frames)[seq_len(nframes)]
    statics2 <- c(statics[length(statics)], statics[-length(statics)])
    previous_static <- rep(statics2, frames)[seq_len(nframes)]
  }

  frames[-1] <- frames[-1] + 1
  closest_static <- tween_constant(as.list(statics[c(seq_along(statics), 1)]), frames)[[1]][seq_len(nframes)]
  info <- data.frame(
    transitioning = phase == 'transition',
    previous_ = previous_static,
    closest_ = closest_static,
    next_ = next_static,
    stringsAsFactors = FALSE
  )
  names(info)[2:4] <- paste0(names(info)[2:4], static_name)
  info
}
