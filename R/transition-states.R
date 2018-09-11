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
#' - **transitioning** is a boolean indicating whether the frame is part of the
#'   transitioning phase
#' - **previous_state** The name of the last state the animation was at
#' - **closest_state** The name of the state closest to this frame
#' - **next_state** The name of the next state the animation will be part of
#'
#' @family transitions
#'
#' @examples
#'
#' p <- ggplot(iris, aes(Sepal.Width, Petal.Width)) +
#'   geom_point() +
#'   labs(title = "{closest_state}") +
#'   transition_states(Species, transition_length = 3, state_length = 1)
#'
#' # animate(p)
#'
#' @export
#' @importFrom rlang enquo
#' @importFrom ggplot2 ggproto
transition_states <- function(states, transition_length, state_length, wrap = TRUE) {
  states_quo <- enquo(states)
  require_quo(states_quo, 'states')
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
TransitionStates <- ggproto('TransitionStates', Transition,
  mapping = '(.*)',
  var_names = 'states',
  setup_params = function(self, data, params) {
    params$states <- get_row_frames(data, params$states_quo, after = FALSE)
    params$require_stat <- is_placeholder(params$states)
    params$row_id <- params$states$values
    params
  },
  setup_params2 = function(self, data, params, row_vars) {
    if (is_placeholder(params$states)) {
      params$states <- get_row_frames(data, params$states_quo, after = TRUE)
    } else {
      params$states$values <- lapply(row_vars$states, as.integer)
    }

    all_levels <- params$states$levels
    row_state <- params$states$values
    transition_length <- rep(params$transition_length, length.out = length(all_levels))
    if (!params$wrap) transition_length[length(transition_length)] <- 0
    state_length <- rep(params$state_length, length.out = length(all_levels))
    frames <- distribute_frames(state_length, transition_length, params$nframes + if (params$wrap) 1 else 0)
    params$state_levels <- all_levels
    params$row_id <- row_state
    params$state_length <- frames$static_length
    params$transition_length <- frames$transition_length
    params$frame_info <- get_frame_info(
      static_levels = params$state_levels,
      static_lengths = params$state_length,
      transition_lengths = params$transition_length,
      nframes = params$nframes,
      static_first = TRUE,
      static_name = 'state')
    params$nframes <- nrow(params$frame_info)
    params
  },
  expand_panel = function(self, data, type, id, match, ease, enter, exit, params, layer_index) {
    row_state <- self$get_row_vars(data)
    if (is.null(row_state)) return(data)
    data$group <- paste0(row_state$before, row_state$after)
    state <- as.integer(row_state$states)
    states <- split(data, state)
    all_states <- rep(list(data[0, ]), length(params$state_levels))
    all_states[as.integer(names(states))] <- states
    all_frames <- all_states[[1]]
    for (i in seq_along(all_states)) {
      if (params$state_length[i] != 0) {
        all_frames <- keep_state(all_frames, params$state_length[i])
      }
      if (params$transition_length[i] != 0) {
        next_state <- if (i == length(all_states)) all_states[[1]] else all_states[[i + 1]]
        all_frames <- switch(
          type,
          point = tween_state(all_frames, next_state, ease, params$transition_length[i], !!id, enter, exit),
          path = transform_path(all_frames, next_state, ease, params$transition_length[i], !!id, enter, exit, match),
          polygon = transform_polygon(all_frames, next_state, ease, params$transition_length[i], !!id, enter, exit, match),
          sf = transform_sf(all_frames, next_state, ease, params$transition_length[i], !!id, enter, exit),
          stop("Unknown layer type", call. = FALSE)
        )
      }
    }
    if (params$wrap) {
      all_frames <- all_frames[all_frames$.frame <= params$nframes, ]
    }
    all_frames$group <- paste0(all_frames$group, '<', all_frames$.frame, '>')
    all_frames$.frame <- NULL
    all_frames
  }
)
