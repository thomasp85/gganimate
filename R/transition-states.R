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
#' literal interpretation, in addition to the general ones provided by
#' [animate()]:
#'
#' - **transitioning** is a boolean indicating whether the frame is part of the
#'   transitioning phase
#' - **previous_state** The name of the last state the animation was at
#' - **closest_state** The name of the state closest to this frame
#' - **next_state** The name of the next state the animation will be part of
#'
#' @section Object permanence:
#' `transition_states` uses the group aesthetic of each layer to identify
#' which rows in the input data correspond to the same graphic element and will
#' therefore define which elements will turn into each other between states.
#' The group aesthetic, if not set, will be calculated from the interaction of all
#' discrete aesthetics in the layer (excluding `label`), so it is often better
#' to set it explicitly when animating, to make sure your data is interpreted in
#' the right way. If the group aesthetic is not set, and no discrete aesthetics
#' exists then all rows will have the same group. If the group aesthetic is not
#' unique in each state, then rows will be matched first by group and then by
#' index. Unmatched rows will appear/disappear, potentially using an enter or
#' exit function.
#'
#' @section Computed Variables:
#' It is possible to use variables calculated by the statistic to define the
#' transition. Simply inclose the variable in `stat()` in the same way as when
#' using computed variables in aesthetics.
#'
#' @family transitions
#'
#' @importFrom rlang enquo
#' @importFrom ggplot2 ggproto
#' @export
#'
#' @examples
#' anim <- ggplot(iris, aes(Sepal.Width, Petal.Width)) +
#'   geom_point() +
#'   labs(title = "{closest_state}") +
#'   transition_states(Species, transition_length = 3, state_length = 1)
#'
#' # Use a unique group to avoid matching graphic elements
#' iris$group <- seq_len(nrow(iris))
#' anim1 <- ggplot(iris, aes(Sepal.Width, Petal.Width, group = group)) +
#'   geom_point() +
#'   labs(title = "{closest_state}") +
#'   transition_states(Species, transition_length = 3, state_length = 1) +
#'   enter_fade() +
#'   exit_fade()
#'
#' # Set `wrap = FALSE` to avoid transitioning the last state to the first
#' anim2 <- ggplot(iris, aes(Sepal.Width, Petal.Width)) +
#'   geom_point() +
#'   labs(title = "{closest_state}") +
#'   transition_states(Species, transition_length = 3, state_length = 1,
#'                     wrap = FALSE)
#'
transition_states <- function(states, transition_length = 1, state_length = 1, wrap = TRUE) {
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
    params$nframes <- sum(frames$static_length) + sum(frames$transition_length)
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
          cli::cli_abort('{type} layers not currently supported by {.fun transition_states}')
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
