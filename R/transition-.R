#' @rdname gganimate-ggproto
#' @format NULL
#' @usage NULL
#' @export
#' @importFrom ggplot2 ggproto
#' @importFrom stringi stri_replace stri_match stri_detect
Transition <- ggproto('Transition', NULL,
  params = NULL,
  mapping = '',
  var_names = '',
  setup_params = function(self, data, params) {
    params
  },
  setup_params2 = function(self, data, params, row_vars) {
    params
  },
  map_data = function(self, data, params, replace = FALSE) {
    if (is.null(params$row_id)) return(data)
    Map(function(d, id) {
      if (length(id) == 0 && !replace) return(d)
      id <- if (length(id) > 0) paste0('<', id, '>') else ''
      d$group <- as.character(d$group)
      d$group <- if (replace && any(stri_detect(d$group, regex = '<.*>'))) {
        stri_replace(d$group, id, regex = '<.*>')
      } else {
        paste0(d$group, id)
      }
      d
    }, d = data, id = params$row_id)
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
    vec_rbind0(!!!expanded)
  },
  expand_panel = function(self, data, type, id, match, ease, enter, exit, params, layer_index) {
    cli::cli_abort('The transition has not implemented any data expansion')
  },
  unmap_frames = function(self, data, params) {
    lapply(data, function(d) {
      split_panel <- stri_match(d$group, regex = '^(.*)(<.*>)(.*)$')
      if (is.na(split_panel[1])) {
        groups <- d$group
      } else {
        groups <- paste0(split_panel[, 2], split_panel[, 4])
        d$PANEL <- paste0(d$PANEL, split_panel[, 3])
      }
      if (all(stri_detect(groups, regex = '^-?[0-9]+$'))) {
        d$group <- as.integer(groups)
      } else {
        d$group <- groups
      }
      d
    })
  },
  remap_frames = function(self, data, params) {
    lapply(data, function(d) {
      split_panel <- stri_match(d$PANEL, regex = '^(.*)(<.*>)(.*)$')
      if (is.na(split_panel[1])) return(d)
      d$PANEL <- as.integer(split_panel[, 2])
      d$group <- paste0(d$group, split_panel[, 3])
      d
    })
  },
  finish_data = function(self, data, params) {
    lapply(data, function(d) {
      split_panel <- stri_match(d$group, regex = '^(.+)<(.*)>(.*)$')
      if (is.na(split_panel[1])) return(list(d))
      d$group <- match(d$group, unique0(d$group))
      empty_d <- d[0, , drop = FALSE]
      d <- split(d, as.integer(split_panel[, 3]))
      frames <- rep(list(empty_d), params$nframes)
      frames[as.integer(names(d))] <- d
      frames
    })
  },
  adjust_nframes = function(self, data, params) {
    statics <- self$static_layers(params)
    dynamics <- setdiff(seq_along(data), statics)
    if (length(dynamics) == 0) {
      params$nframes
    } else {
      length(data[[dynamics[1]]])
    }
  },
  get_frame_data = function(self, data, params, i) {
    statics <- seq_along(data) %in% self$static_layers(params)
    Map(function(d, s) {
      if (s) d else d[i]
    }, d = data, s = statics)
  },
  get_frame_vars = function(self, params) {
    params$frame_info
  },
  static_layers = function(self, params) {
    which(lengths(params$row_id) == 0)
  },
  require_late_tween = function(self, params) {
    isTRUE(params$require_stat)
  },
  get_row_vars = function(self, data) {
    vars <- stri_match(data$group, regex = paste0('(.*)<', self$mapping, '>(.*)'))[, -1, drop = FALSE]
    if (is.na(vars[1])) return(NULL)
    var_names <- c('before', self$var_names, 'after')
    structure(lapply(seq_along(var_names), function(i) vars[,i]), names = var_names)
  },
  get_all_row_vars = function(self, data) {
    all_vars <- lapply(data, self$get_row_vars)
    var_names <- unique0(unlist(lapply(all_vars, names)))
    structure(lapply(var_names, function(var) {
      lapply(all_vars, `[[`, var)
    }), names = var_names)
  }
)
#' @importFrom ggplot2 ggplot_add
#' @export
ggplot_add.Transition <- function(object, plot, ...) {
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
  zero_trans <- which(transition_frames == 0 & static_frames[which(transition_frames == 0) + 1] != 0)
  transition_frames[zero_trans] <- 1
  static_frames[zero_trans + 1] <- static_frames[zero_trans + 1] - 1
  list(static_length = static_frames, transition_length = transition_frames, mod = frames / total)
}

#' @importFrom tweenr tween_constant
get_frame_info <- function(static_levels, static_lengths, transition_lengths, nframes, static_first, static_name, ...) {
  if (static_first) {
    frames <- vctrs::vec_interleave(static_lengths, transition_lengths)
    phase <- rep(rep(c('static', 'transition'), length(static_lengths)), frames)[seq_len(nframes)]
  } else {
    frames <- vctrs::vec_interleave(transition_lengths, static_lengths)
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
  info <- data_frame0(
    transitioning = phase == 'transition',
    previous_ = previous_static,
    closest_ = closest_static,
    next_ = next_static,
    stringsAsFactors = FALSE
  )
  names(info)[2:4] <- paste0(names(info)[2:4], static_name)
  info
}
standardise_times <- function(times, name, to_class = NULL) {
  possible_classes <- c('integer', 'numeric', 'POSIXct', 'Date', 'difftime', 'hms')
  classes <- vapply(times[lengths(times) != 0], function(x) {
    cl <- inherits(x, possible_classes, which = TRUE)
    which(cl != 0 & cl == min(cl[cl != 0]))[1]
  }, integer(1))
  if (anyNA(classes)) cli::cli_abort('{name} data must either be an {.or {possible_classes}} object')
  if (vctrs::vec_unique_count(classes) > 1) cli::cli_abort('{name} data must be the same class in all layers')
  cl <- possible_classes[unique0(classes)]
  if (length(cl) == 1 && (cl == 'difftime' || cl == 'hms')) {
    if (is.null(to_class)) {
      times <- lapply(times, `units<-`, 'secs')
    } else if (to_class == 'POSIXct') {
      cl <- to_class
      times <- lapply(times, `units<-`, 'secs')
    } else if (to_class == 'Date') {
      if (cl == 'hms') {
        times <- lapply(times, `class<-`, 'difftime')
      }
      times <- lapply(times, `units<-`, 'days')
      cl <- to_class

    }
  }
  if (!is.null(to_class) && length(cl) != 0 && cl != to_class) cli::cli_abort('{name} data must be {.cls {to_class}}')
  list(
    times = lapply(times, as.numeric),
    class = cl
  )
}
recast_times <- function(time, class) {
  switch(
    class,
    integer = as.integer(round(time)),
    numeric =  time,
    POSIXct = structure(time, class = c('POSIXct', 'POSIXt')),
    Date = structure(time, class = 'Date'),
    difftime = structure(time, units = 'secs', class = 'difftime'),
    hms = structure(time, units = 'secs', class = c('hms','difftime'))
  )
}
eval_placeholder <- function(data) {
  structure(list(values = lapply(data, function(d) rep('*', nrow(d)))), class = 'placeholder')
}
is_placeholder <- function(x) inherits(x, 'placeholder')
