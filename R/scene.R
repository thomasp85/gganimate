#' @importFrom ggplot2 ggproto
create_scene <- function(transition, view, shadow, ease, transmuters, nframes) {
  if (is.null(nframes)) nframes <- 100
  ggproto(NULL, Scene, transition = transition, view = view, shadow = shadow, ease = ease, transmuters = transmuters, nframes = nframes)
}
#' @importFrom ggplot2 ggproto ggplot_gtable labs
#' @importFrom glue glue_data
#' @importFrom grid grid.newpage grid.draw seekViewport pushViewport upViewport
Scene <- ggproto('Scene', NULL,
  transition = NULL,
  view = NULL,
  shadow = NULL,
  ease = NULL,
  transmuters = NULL,
  nframes = integer(),
  transition_params = list(),
  view_params = list(),
  shadow_params = list(),
  layer_type = character(),
  tween_first = logical(),
  group_column = character(),
  match_shape = logical(),

  setup = function(self, layer_data) {
    transition_params <- self$transition$params
    transition_params$nframes <- self$nframes
    self$transition_params <- self$transition$setup_params(layer_data, transition_params)
    view_params <- self$view$params
    view_params$nframes <- self$nframes
    self$view_params <- self$view$setup_params(layer_data, view_params)
    shadow_params <- self$shadow$params
    shadow_params$nframes <- self$nframes
    self$shadow_params <- self$shadow$setup_params(layer_data, shadow_params)
  },
  identify_layers = function(self, layer_data, layers) {
    self$transmuters$setup(layers)
    self$layer_type <- self$get_layer_type(layer_data, layers)
    self$tween_first <- self$is_early_tween(layers)
    if (self$transition$require_late_tween(self$transition_params)) self$tween_first[] <- FALSE
    self$group_column <- self$get_group_column(layers)
    self$match_shape <- self$get_shape_match(layers)
    self$transition_params$stat_align_layer <- vapply(layers, function(l) inherits(l$stat, 'StatAlign'), logical(1))
  },
  before_stat = function(self, layer_data) {
    layer_data <- self$transition$map_data(layer_data, self$transition_params)
    ease <- self$ease$get_ease(layer_data[self$tween_first])
    layer_data[self$tween_first] <- self$transition$expand_data(
      layer_data[self$tween_first],
      self$layer_type[self$tween_first],
      self$group_column[self$tween_first],
      self$match_shape[self$tween_first],
      ease,
      self$transmuters$enter_transmuters(self$tween_first),
      self$transmuters$exit_transmuters(self$tween_first),
      self$transition_params,
      which(self$tween_first)
    )
    layer_data
  },
  after_stat = function(self, layer_data) {
    row_vars <- self$transition$get_all_row_vars(layer_data)
    self$transition_params <- self$transition$setup_params2(layer_data, self$transition_params, row_vars)
    layer_data <- self$transition$map_data(layer_data, self$transition_params, replace = TRUE)
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
      self$group_column[tween_last],
      self$match_shape[tween_last],
      ease,
      self$transmuters$enter_transmuters(tween_last),
      self$transmuters$exit_transmuters(tween_last),
      self$transition_params,
      which(tween_last)
    )
    layer_data
  },
  finish_data = function(self, layer_data) {
    layer_data <- self$transition$finish_data(layer_data, self$transition_params)
    self$nframes <- self$transition$adjust_nframes(layer_data, self$transition_params)
    static_layers <- self$transition$static_layers(self$transition_params)
    self$view_params$nframes <- self$nframes
    self$view_params$excluded_layers <- union(self$view$exclude_layer, static_layers)
    self$view_params <- self$view$train(layer_data, self$view_params)
    self$shadow_params$nframes <- self$nframes
    self$shadow_params$excluded_layers <- union(self$shadow$exclude_layer, static_layers)
    self$shadow_params <- self$shadow$train(layer_data, self$shadow_params)
    frame_vars <- list(
      data_frame0(frame = seq_len(self$nframes), nframes = self$nframes, progress = seq_len(self$nframes)/self$nframes),
      self$transition$get_frame_vars(self$transition_params),
      self$view$get_frame_vars(self$view_params)
    )
    self$frame_vars <- vctrs::vec_cbind(!!!frame_vars, .name_repair = 'minimal')
    layer_data
  },
  get_frame = function(self, plot, i) {
    data <- self$transition$get_frame_data(plot$data, self$transition_params, i)
    shadow_i <- self$shadow$get_frames(self$shadow_params, i)
    shadow <- self$transition$get_frame_data(plot$data, self$transition_params, shadow_i)
    shadow <- self$shadow$prepare_shadow(shadow, self$shadow_params)
    plot$data <- self$shadow$prepare_frame_data(data, shadow, self$shadow_params, i, shadow_i)
    plot <- self$view$set_view(plot, self$view_params, i)
    plot <- self$set_labels(plot, i)
    PANEL_levels <- plot$layout$layout$PANEL
    plot$data <- lapply(plot$data, function(d) {
      d$PANEL <- factor(d$PANEL, PANEL_levels)
      d
    })
    plot
  },
  plot_frame = function(self, plot, i, newpage = is.null(vp), vp = NULL, widths = NULL, heights = NULL, ...) {
    plot <- self$get_frame(plot, i)
    plot <- render_frame(plot)
    if (!is.null(widths)) plot$widths <- widths
    if (!is.null(heights)) plot$heights <- heights
    if (newpage) grid.newpage()
    grDevices::recordGraphics(
      requireNamespace("gganimate", quietly = TRUE),
      list(),
      getNamespace("gganimate")
    )
    if (is.null(vp)) {
      grid.draw(plot)
    } else {
      if (is_character(vp)) seekViewport(vp)
      else pushViewport(vp)
      grid.draw(plot)
      upViewport()
    }
    invisible(NULL)
  },
  set_labels = function(self, plot, i) {
    label_var <- as.list(self$frame_vars[i, ])
    label_var$data <- plot$data
    plot$plot$labels <- labs(!!!lapply(plot$plot$labels, function(label) {
      orig_call <- FALSE
      if (is_call(label)) {
        orig_call <- TRUE
        label <- list(label)
      }
      new_label <- lapply(label, function(l) {
        l2 <- try(glue_call(label_var, l, .envir = plot$plot$plot_env), silent = TRUE)
        if (inherits(l2, 'try-error')) {
          l
        } else {
          l2
        }
      })
      if (orig_call) {
        new_label[[1]]
      } else if (is_expression(label)) {
        as.expression(new_label)
      } else {
        unlist(new_label)
      }
    }))
    plot
  },
  get_group_column = function(self, layers) {
    lapply(layers, function(l) {
      group_column(l$stat) %||% group_column(l$geom) %||% quo(group)
    })
  },
  get_layer_type = function(self, data, layers) {
    unlist(Map(function(l, d) {
      layer_type(l$stat) %||% layer_type(l$geom) %||% layer_type(d) %||% 'point'
    }, l = layers, d = data))
  },
  get_shape_match = function(self, layers) {
    vapply(layers, function(l) {
      match_shapes(l$stat) %||% match_shapes(l$geom) %||% TRUE
    }, logical(1))
  },
  is_early_tween = function(self, layers) {
    vapply(layers, tween_before_stat, logical(1))
  }
)

glue_call <- function(.x, call, .envir) {
  if (is_string(call)) {
    return(glue_data(.x, call, .envir = .envir))
  }
  if (is_call(call)) {
    call[] <- lapply(call, glue_call, .x = .x, .envir = .envir)
  }
  call
}
