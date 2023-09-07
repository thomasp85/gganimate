#' @include transition-manual.R
NULL

#' Build up a plot, layer by layer
#'
#' This transition gradually adds layers to the plot in the order they have been
#' defined. By default prior layers are kept for the remainder of the animation,
#' but they can also be set to be removed as the next layer enters.
#'
#' @param layer_length The proportional time to pause at each layer before a new
#' one enters
#' @param transition_length The proportional time to use for the entrance of a
#' new layer
#' @param keep_layers Either an integer indicating for how many following layers
#' the layers should stay on screen or a logical. In the case of the later,
#' `TRUE` will mean keep the layer for the remainder of the animation
#' (equivalent to setting it to `Inf`) and `FALSE` will mean to transition the
#' layer out as the next layer enters.
#' @param from_blank Should the first layer transition in or be present on the
#' onset of the animation
#' @param layer_order An alternative order the layers should appear in (default
#' to using the stacking order). All other arguments that references the layers
#' index in some way refers to this order.
#' @param layer_names A character vector of names for each layers, to be used
#' when interpreting label literals
#'
#' @section Label variables:
#' `transition_layers` makes the following variables available for string
#' literal interpretation, in addition to the general ones provided by
#' [animate()]:
#'
#' - **transitioning** is a boolean indicating whether the frame is part of the
#'   transitioning phase
#' - **previous_layer** The name of the last layer the animation was showing
#' - **closest_layer** The name of the layer the animation is closest to showing
#' - **next_layer** The name of the next layer the animation will show
#' - **nlayers** The total number of layers
#'
#' @section Object permanence:
#' `transition_layer` does not link rows across data to the same graphic
#' element, so elements will be defined uniquely by each row and the enter and
#' exit of the layer it belongs to.
#'
#' @family transitions
#'
#' @importFrom ggplot2 ggproto
#' @export
#'
#' @examples
#' # Default is to use layer order and keep layers for duration of animation
#' anim <- ggplot(mtcars, aes(mpg, disp)) +
#'   geom_point() +
#'   geom_smooth(colour = 'grey', se = FALSE) +
#'   geom_smooth(aes(colour = factor(gear))) +
#'   transition_layers(layer_length = 1, transition_length = 2) +
#'   enter_fade() + enter_grow()
#'
#' # Start with the first layer already present
#' anim1 <- ggplot(mtcars, aes(mpg, disp)) +
#'   geom_point() +
#'   geom_smooth(colour = 'grey', se = FALSE) +
#'   geom_smooth(aes(colour = factor(gear))) +
#'   transition_layers(layer_length = 1, transition_length = 2,
#'                     from_blank = FALSE) +
#'   enter_fade() + enter_grow()
#'
#' # Change the order of the layers
#' anim2 <- ggplot(mtcars, aes(mpg, disp)) +
#'   geom_point() +
#'   geom_smooth(colour = 'grey', se = FALSE) +
#'   geom_smooth(aes(colour = factor(gear))) +
#'   transition_layers(layer_length = 1, transition_length = 2,
#'                     from_blank = FALSE, layer_order = c(3, 1, 2)) +
#'   enter_fade() + enter_grow()
#'
#' # Keep layer 1 for the whole animation, but remove the 2nd layer as the 3rd
#' # enters
#' anim3 <- ggplot(mtcars, aes(mpg, disp)) +
#'   geom_point() +
#'   geom_smooth(colour = 'grey', se = FALSE) +
#'   geom_smooth(aes(colour = factor(gear))) +
#'   transition_layers(layer_length = 1, transition_length = 2,
#'                     from_blank = FALSE, keep_layers = c(Inf, 0, 0)) +
#'   enter_fade() + enter_grow() +
#'   exit_fade() + exit_shrink()
#'
transition_layers <- function(layer_length = 1, transition_length = 1, keep_layers = TRUE, from_blank = TRUE, layer_order = NULL, layer_names = NULL) {
  if (is_logical(keep_layers)) keep_layers <- if (keep_layers) Inf else 0L
  ggproto(NULL, TransitionLayers,
    params = list(
      layer_length = layer_length,
      transition_length = transition_length,
      keep_layers = keep_layers,
      from_blank = from_blank,
      layer_order = layer_order,
      layer_names = layer_names
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
TransitionLayers <- ggproto('TransitionLayers', Transition,
  setup_params = function(self, data, params) {
    params$nlayers <- length(data)
    params$layer_order <- if (is.null(params$layer_order)) {
      seq_len(params$nlayers)
    } else {
      match(seq_along(data), params$layer_order)
    }
    layer_length <- rep_len(params$layer_length, length(data))
    transition_length <- rep_len(params$transition_length, length(data) + params$from_blank)
    params$keep_layers <- rep_len(params$keep_layers, length(data))
    if (params$from_blank) layer_length <- c(0, layer_length)
    transitions_out <- !is.infinite(params$keep_layers[length(data)])
    if (!transitions_out) {
      transition_length[length(transition_length)] <- 0
    } else {
      transition_length <- c(transition_length, 0)
      layer_length <- c(layer_length, 0)
    }
    frames <- distribute_frames(layer_length, transition_length, params$nframes)
    frames <- data_frame0(layer = frames$static_length, exit = frames$transition_length)
    frames$enter <- c(0, frames$exit[-nrow(frames)])
    if (params$from_blank) frames <- frames[-1, ]
    params$offset <- c(0, cumsum(frames$enter + frames$layer)[-nrow(frames)])
    params$enter_length <- frames$enter
    params$layer_length <- frames$layer
    params$exit_length <- frames$exit
    if (is.null(params$layer_names)) {
      params$layer_names <- as.character(seq_along(data))
    } else {
      if (length(params$layer_names) != length(data)) {
        cli::cli_abort('When providing layer names the number of names must match the number of layers')
      }
    }
    if (transitions_out) {
      params$layer_names <- params$layer_names[c(seq_along(data), length(data))]
    }
    params$frame_info <- get_frame_info(
      static_levels = params$layer_names,
      static_lengths = params$layer_length,
      transition_lengths = params$enter_length,
      nframes = params$nframes,
      static_first = FALSE,
      static_name = 'layer')
    params$frame_info$nlayers <- params$nlayers
    params$nframes <- nrow(params$frame_info)
    params
  },
  expand_layer = function(self, data, type, id, match, ease, enter, exit, params, layer_index) {
    layer_index <- params$layer_order[layer_index]
    offset <- params$offset[layer_index]
    enter_length <- params$enter_length[layer_index]
    if (enter_length < 1) enter_length <- 1
    exit_length <- params$exit_length[layer_index]
    layer_length <- params$layer_length[layer_index]
    max_length <- params$nframes - offset - enter_length
    layer_length <- if (is.infinite(params$keep_layers[layer_index])) {
      max_length
    } else if (params$keep_layers[layer_index] != 0 && layer_index != params$nlayers) {
      last_layer <- min(layer_index + params$keep_layers[layer_index], params$nlayers)
      params$offset[last_layer] + params$layer_length[last_layer] - offset
    } else {
      layer_length
    }
    layer <- switch(
      type,
      point = tween_state(data[0,], data, ease, enter_length, NULL, enter, exit),
      path = transform_path(data[0,], data, ease, enter_length, NULL, enter, exit),
      polygon = transform_polygon(data[0,], data, ease, enter_length, NULL, enter, exit),
      sf = transform_sf(data[0,], data, ease, enter_length, NULL, enter, exit),
      cli::cli_abort('{type} layers not currently supported by {.fun transition_layers}')
    )
    layer <- keep_state(layer, layer_length)
    if (is.finite(params$keep_layers[layer_index])) {
      layer <- switch(
        type,
        point = tween_state(layer, data[0,], ease, exit_length, NULL, enter, exit),
        path = transform_path(layer, data[0,], ease, exit_length, NULL, enter, exit),
        polygon = transform_polygon(layer, data[0,], ease, exit_length, NULL, enter, exit),
        sf = transform_sf(layer, data[0,], ease, exit_length, NULL, enter, exit),
        cli::cli_abort('{type} layers not currently supported by {.fun transition_layers}')
      )
    }
    layer <- layer[layer$.frame <= params$nframes, , drop = FALSE]
    layer$group <- paste0(layer$group, '<', layer$.frame + offset, '>')
    layer$.frame <- NULL
    layer
  },
  static_layers = function(self, params) {
    numeric(0)
  }
)
