#' Transition between several distinct stages of the data
#'
#' This transition splits your data into multiple states based on the levels in
#' a given column, much like [ggplot2::facet_wrap()] splits up the data in
#' multiple panels. It then tweens between the defined states and pauses at each
#' state. Layers with data without the specified column will be kept constant
#' during the animation (again, mimicking `facet_wrap`).
#'
#' @param states The unquoted name of the column holding the state levels in the
#' data.
#' @param transition_length The relative length of the transition. Will be
#' recycled to match the number of states in the data
#' @param state_length The relative length of the pause at the states. Will be
#' recycled to match the number of states in the data
#' @param wrap Should the animation *wrap-around*? If `TRUE` the last state will
#' be transitioned into the first.
#'
#' @section Label variables:
#' `transition_states` makes the following variables available for string
#' literal interpretation:
#'
#' - **transitioning** is a booloean indicating whether the frame is part of the
#'   transitioning phase
#' - **previous_state** The name of the last state the animation was at
#' - **closest_state** The name of the state closest to this frame
#' - **next_state** The name of the next state the animation will be part of
#'
#' @family transitions
#'
#' @export
#' @importFrom rlang enquo
#' @importFrom ggplot2 ggproto
transition_states <- function(states, transition_length, state_length, wrap = TRUE) {
  states_quo <- enquo(states)
  ggproto(NULL, TransitionStates,
    params = list(
      states_quo = states_quo,
      transition_length = transition_length,
      state_length = state_length,
      wrap = wrap
    )
  )
}
#' @rdname gganimate-ggproto
#' @format NULL
#' @usage NULL
#' @export
#' @importFrom ggplot2 ggproto
#' @importFrom stringi stri_match
#' @importFrom tweenr tween_state keep_state
#' @importFrom transformr tween_path tween_polygon tween_sf
TransitionStates <- ggproto('TransitionStates', Transition,
  setup_params = function(self, data, params) {
    states <- combine_levels(data, params$states_quo)
    all_levels <- states$levels
    row_state <- states$values
    transition_length <- rep(params$transition_length, length.out = length(all_levels))
    if (!params$wrap) transition_length[length(transition_length)] <- 0
    state_length <- rep(params$state_length, length.out = length(all_levels))
    frames <- distribute_frames(state_length, transition_length, params$nframes + if (params$wrap) 1 else 0)
    params$state_levels <- all_levels
    params$row_state <- row_state
    params$state_length <- frames$state_length
    params$transition_length <- frames$transition_length
    params$frame_info <- do.call(get_frame_info, params)
    params
  },
  map_data = function(self, data, params) {
    Map(function(d, id) {
      if (length(id) > 0) {
        d$group <- paste0(d$group, '_', id)
      }
      d
    }, d = data, id = params$row_state)
  },
  expand_data = function(self, data, type, ease, enter, exit, params, layer_index) {
    Map(function(d, t, en, ex, es) {
      split_panel <- stri_match(d$group, regex = '^(.+)_(.+)$')
      if (is.na(split_panel[1])) return(d)
      d$group <- as.integer(split_panel[, 2])
      state <- as.integer(split_panel[, 3])
      states <- split(d, state)
      all_states <- rep(list(d[0, ]), length(params$state_levels))
      all_states[as.integer(names(states))] <- states
      all_frames <- all_states[[1]]
      id <- if (d$group[1] == -1) NULL else 'group'
      for (i in seq_along(all_states)) {
        if (params$state_length[i] != 0) {
          all_frames <- keep_state(all_frames, params$state_length[i])
        }
        if (params$transition_length[i] != 0) {
          next_state <- if (i == length(all_states)) all_states[[1]] else all_states[[i + 1]]
          all_frames <- switch(
            t,
            point = tween_state(all_frames, next_state, es, params$transition_length[i], id, en, ex),
            path = tween_path(all_frames, next_state, es, params$transition_length[i], id, en, ex),
            polygon = tween_polygon(all_frames, next_state, es, params$transition_length[i], id, en, ex),
            sf = tween_sf(all_frames, next_state, es, params$transition_length[i], id, en, ex),
            stop("Unknown layer type", call. = FALSE)
          )
        }
      }
      if (params$wrap) {
        all_frames <- all_frames[all_frames$.frame <= params$nframes, ]
      }
      all_frames$group <- paste0(all_frames$group, '_', all_frames$.frame)
      all_frames$.frame <- NULL
      all_frames
    }, d = data, t = type, en = enter, ex = exit, es = ease)
  },
  unmap_frames = function(self, data, params) {
    lapply(data, function(d) {
      split_panel <- stri_match(d$group, regex = '^(.+)_(.+)$')
      if (is.na(split_panel[1])) return(d)
      d$group <- as.integer(split_panel[, 2])
      d$PANEL <- paste0(d$PANEL, '_', split_panel[, 3])
      d
    })
  },
  remap_frames = function(self, data, params) {
    lapply(data, function(d) {
      split_panel <- stri_match(d$PANEL, regex = '^(.+)_(.+)$')
      if (is.na(split_panel[1])) return(d)
      d$PANEL <- as.integer(split_panel[, 2])
      d$group <- paste0(d$group, '_', split_panel[, 3])
      d
    })
  },
  finish_data = function(self, data, params) {
    lapply(data, function(d) {
      split_panel <- stri_match(d$group, regex = '^(.+)_(.+)$')
      if (is.na(split_panel[1])) return(rep(list(d), params$nframes))
      split(d, as.integer(split_panel[, 3]))
    })
  },
  adjust_nframes = function(self, data, params) {
    length(data[[1]])
  },
  add_label_vars = function(self, var, i, params, plot) {
    c(var, as.list(params$frame_info[i, ]))
  }
)


# HELPERS -----------------------------------------------------------------

#' @importFrom rlang eval_tidy
combine_levels <- function(data, var) {
  values <- lapply(data, eval_tidy, expr = var)
  values[lengths(values) != vapply(data, nrow, integer(1))] <- list(NULL)
  levels <- lapply(values, function(v) levels(as.factor(v)))
  levels <- Reduce(union, levels)
  values <- lapply(values, as.character)
  values <- split(match(unlist(values), levels), rep(seq_along(values), lengths(values)))
  list(values = values, levels = levels)
}
distribute_frames <- function(states, transitions, frames) {
  total <- sum(c(states, transitions))
  state_frames <- ceiling(states * frames / total)
  transition_frames <- ceiling(transitions * frames / total)
  all <- c(state_frames, transition_frames)
  n <- rep(seq_along(all), all)
  ind <- unlist(lapply(all, seq_len))
  n <- table(n[order(ind)[seq_len(frames)]])
  ind <- as.integer(names(n))
  state_numbers <- ind <= length(states)
  state_frames[ind[state_numbers]] <- n[state_numbers]
  transition_frames[ind[!state_numbers] - length(states)] <- n[!state_numbers]
  list(state_length = state_frames, transition_length = transition_frames, mod = frames / total)
}
#' @importFrom tweenr tween_constant
get_frame_info <- function(state_levels, state_lengths, transition_lengths, nframes, ...) {
  frames <- as.vector(rbind(state_lengths, transition_lengths))
  phase <- rep(rep(c('constant', 'transition'), length(state_lengths)), frames)[seq_len(nframes)]
  states <- rep(state_levels, each = 2)
  previous_state <- rep(states, frames)[seq_len(nframes)]
  states2 <- c(states[-1], states[1])
  next_state <- rep(states2, frames)[seq_len(nframes)]
  frames[-1] <- frames[-1] + 1
  closest_state <- tween_constant(as.list(states[c(seq_along(states), 1)]), frames)[[1]][seq_len(nframes)]
  data.frame(
    transitioning = phase == 'transition',
    previous_state = previous_state,
    closest_state = closest_state,
    next_state = next_state,
    stringsAsFactors = FALSE
  )
}
