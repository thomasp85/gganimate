#' @importFrom ggplot2 ggproto
create_scene <- function(transition, view, ease, transmuters, nframes) {
  if (is.null(nframes)) nframes <- 100
  ggproto(NULL, Scene, transition = transition, view = view, ease = ease, transmuters = transmuters, nframes = nframes)
}
#' @importFrom ggplot2 ggproto
#' @importFrom glue glue_data
Scene <- ggproto('Scene', NULL,
  transition = NULL,
  view = NULL,
  ease = NULL,
  transmuters = NULL,
  nframes = integer(),
  transition_params = list(),
  view_params = list(),
  layer_type = character(),
  tween_first = logical(),

  setup = function(self, layer_data) {
    transition_params <- self$transition$params
    transition_params$nframes <- self$nframes
    self$transition_params <- self$transition$setup_params(layer_data, transition_params)
    view_params <- self$view$params
    view_params$nframes <- self$nframes
    self$view_params <- self$view$setup_params(layer_data, view_params)
  },
  identify_layers = function(self, layer_data, layers) {
    self$transmuters$setup(layers)
    self$layer_type = self$get_layer_type(layer_data, layers)
    self$tween_first = self$is_early_tween(layers)
  },
  before_stat = function(self, layer_data) {
    layer_data <- self$transition$map_data(layer_data, self$transition_params)
    ease <- self$ease$get_ease(layer_data[self$tween_first])
    layer_data[self$tween_first] <- self$transition$expand_data(
      layer_data[self$tween_first],
      self$layer_type[self$tween_first],
      ease,
      self$transmuters$enter_transmuters(self$tween_first),
      self$transmuters$exit_transmuters(self$tween_first),
      self$transition_params
    )
    layer_data
  },
  after_stat = function(self, layer_data) {
    layer_data
  },
  before_position = function(self, layer_data) {
    self$transition$unmap_frames(layer_data, self$transition_params)
  },
  after_position = function(self, layer_data) {
    self$transition$remap_frames(layer_data, self$transition_params)
  },
  after_defaults = function(self, layer_data) {
    tween_last <- !self$tween_first
    ease <- self$ease$get_ease(layer_data[tween_last])
    layer_data[tween_last] <- self$transition$expand_data(
      layer_data[tween_last],
      self$layer_type[tween_last],
      ease,
      self$transmuters$enter_transmuters(self$tween_last),
      self$transmuters$exit_transmuters(self$tween_last),
      self$transition_params
    )
    layer_data
  },
  finish_data = function(self, layer_data) {
    layer_data <- self$transition$finish_data(layer_data, self$transition_params)
    self$nframes <- self$transition$adjust_nframes(layer_data, self$transition_params)
    self$view_params$nframes <- self$nframes
    self$view_params <- self$view$train(layer_data, self$view_params)
    layer_data
  },
  get_frame = function(self, plot, i) {
    class(plot) <- 'ggplot_built'
    plot <- self$transition$set_data(plot, self$transition_params, i)
    plot <- self$view$set_view(plot, self$view_params, i)
    plot <- self$set_labels(plot, i)
    plot
  },
  set_labels = function(self, plot, i) {
    label_var <- list(frame = i, nframes = self$nframes, progress = i/self$nframes, data = plot$data)
    label_var <- self$transition$add_label_vars(label_var, i, self$transition_params, plot)
    label_var <- self$view$add_label_vars(label_var, i, self$view_params, plot)
    plot$plot$labels <- lapply(plot$plot$labels, glue_data, .x = label_var, .envir = plot$plot$plot_env)
    plot
  },
  get_layer_type = function(self, data, layers) {
    unlist(Map(function(l, d) {
      layer_type(l$stat) %||% layer_type(l$geom) %||% layer_type(d)
    }, l = layers, d = data))
  },
  is_early_tween = function(self, layers) {
    vapply(layers, tween_before_stat, logical(1))
  }
)
