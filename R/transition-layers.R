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
#' @param transition_length The proportional time to usse for the entrance of a
#' new layer
#' @param keep_layers Should layers be kept on screen after they have appeeared
#' or transition out when a new layer enters
#' @param from_blank Should the first layer transition in or be present on the
#' onset of the animation
#' @param layer_names A character vector of names for each layers, to be used
#' when interpreting label literals
#'
#' @section Label variables:
#' `transition_layers` makes the following variables available for string
#' literal interpretation:
#'
#' - **transitioning** is a booloean indicating whether the frame is part of the
#'   transitioning phase
#' - **previous_layer** The name of the last layer the animation was showing
#' - **closest_layer** The name of the layer the animation is closest to showing
#' - **next_layer** The name of the next layer the animation will show
#' - **nlayers** The total number of layers
#'
#' @family transitions
#'
#' @export
transition_layers <- function(layer_length, transition_length, keep_layers = TRUE, from_blank = TRUE, layer_names = NULL) {
  ggproto(NULL, TransitionLayers,
    params = list(
      layer_length = layer_length,
      transition_length = transition_length,
      keep_layers = keep_layers,
      from_blank = from_blank,
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
#' @importFrom transformr tween_path tween_polygon tween_sf
TransitionLayers <- ggproto('TransitionLayers', TransitionManual,
  setup_params = function(self, data, params) {
    layer_length <- rep(params$layer_length, length.out = length(data))
    transition_length <- rep(params$transition_length, length.out = length(data) + params$from_blank)
    if (params$from_blank) layer_length <- c(0, layer_length)
    if (params$keep_layers) {
      transition_length[length(transition_length)] <- 0
    }
    frames <- distribute_frames(layer_length, transition_length, params$nframes)
    frames <- data.frame(layer = frames$static_length, exit = frames$transition_length)
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
        stop('When providing layer names the number of names must match the number of layers', call. = FALSE)
      }
    }
    params$frame_info <- get_frame_info(
      static_levels = params$layer_names,
      static_lengths = params$layer_length,
      transition_lengths = params$enter_length,
      nframes = params$nframes,
      static_first = FALSE,
      static_name = 'layer')
    params$frame_info$nlayers <- length(data)
    params$nframes <- nrow(params$frame_info)
    params
  },
  map_data = function(self, data, params) {
    data
  },
  expand_data = function(self, data, type, ease, enter, exit, params, layer_index) {
    Map(function(d, t, en, ex, es, en_l, ex_l, ke_l, offset) {
      if (params$keep_layers) ke_l <- params$nframes - offset - en_l
      layer <- switch(
        t,
        point = tween_state(d[0,], d, es, en_l, NULL, en, ex),
        path = tween_path(d[0,], d, es, en_l, NULL, en, ex),
        polygon = tween_polygon(d[0,], d, es, en_l, NULL, en, ex),
        sf = tween_sf(d[0,], d, es, en_l, NULL, en, ex),
        stop("Unknown layer type", call. = FALSE)
      )
      layer <- keep_state(layer, ke_l)
      if (!params$keep_layers) {
        layer <- switch(
          t,
          point = tween_state(layer, d[0,], es, en_l, NULL, en, ex),
          path = tween_path(layer, d[0,], es, en_l, NULL, en, ex),
          polygon = tween_polygon(layer, d[0,], es, en_l, NULL, en, ex),
          sf = tween_sf(layer, d[0,], es, en_l, NULL, en, ex),
          stop("Unknown layer type", call. = FALSE)
        )
      }
      layer$group <- paste0(layer$group, '_', layer$.frame + offset)
      layer$.frame <- NULL
      layer
    }, d = data, t = type, en = enter, ex = exit, es = ease, en_l = params$enter_length[layer_index], ex_l = params$exit_length[layer_index], ke_l = params$layer_length[layer_index], offset = params$offset[layer_index])
  },
  static_layers = function(self, params) {
    numeric(0)
  }
)
