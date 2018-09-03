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
  expand_data = function(self, data, type, id, match, ease, enter, exit, params, layer_index) {
    Map(function(data, type, id, match, ease, enter, exit, layer_index) {
      self$expand_layer(data, type, id, match, ease, enter, exit, params, layer_index)
    }, data = data, type = type, id = id, match = match, ease = ease, enter = enter, exit = exit, layer_index = layer_index)
  },
  expand_layer = function(self, data, type, id, match, ease, enter, exit, params, layer_index) {
    expanded <- lapply(split(data, data$PANEL), function(data) {
      self$expand_panel(data, type, id, match, ease, enter, exit, params, layer_index)
    })
    do.call(rbind, expanded)
  },
  expand_panel = function(self, data, type, id, match, ease, enter, exit, params, layer_index) {
    stop('The transition has not implemented any data expansion', call. = FALSE)
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
  adjust_nframes = function(self, data, params) {
    params$nframes
  },
  get_frame_data = function(self, data, params, i) {
    statics <- seq_along(data) %in% self$static_layers(params)
    Map(function(d, s) {
      if (s) list(d) else d[i]
    }, d = data, s = statics)
  },
  get_frame_vars = function(self, params) {
    NULL
  },
  static_layers = function(self, params) {
    numeric(0)
  }
)
#' @importFrom ggplot2 ggplot_add
#' @export
ggplot_add.Transition <- function(object, plot, object_name) {
  plot <- as.gganim(plot)
  plot$transition <- object
  plot
}

# HELPERS -----------------------------------------------------------------

combine_levels <- function(data, var) {
  values <- lapply(data, safe_eval, expr = var)
  values[lengths(values) != vapply(data, nrow, integer(1))] <- list(numeric())
  levels <- lapply(values, function(v) levels(as.factor(v)))
  levels <- Reduce(union, levels)
  values <- lapply(values, as.character)
  values <- split(match(unlist(values), levels), rep(factor(seq_along(values)), lengths(values)))
  list(values = values, levels = levels)
}

distribute_frames <- function(statics, transitions, frames) {
  total <- sum(c(statics, transitions))
  static_frames <- ceiling(statics * frames / total)
  transition_frames <- ceiling(transitions * frames / total)
  all <- c(static_frames, transition_frames)
  n <- rep(seq_along(all), all)
  ind <- unlist(lapply(all, seq_len))
  n <- table(n[order(ind)[seq_len(frames)]])
  ind <- as.integer(names(n))
  static_numbers <- ind <= length(statics)
  static_frames[ind[static_numbers]] <- n[static_numbers]
  transition_frames[ind[!static_numbers] - length(statics)] <- n[!static_numbers]
  list(static_length = static_frames, transition_length = transition_frames, mod = frames / total)
}

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

  closest_static <- tween_constant(as.list(statics[c(seq_along(statics), 1)]), frames + 1)[[1]][seq_len(nframes)]
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
