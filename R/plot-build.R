#' @importFrom ggplot2 ggplot_build geom_blank waiver
#' @export
ggplot_build.gganim <- function(plot, ...) {
  plot <- plot_clone(plot)
  if (length(plot$layers) == 0) {
    plot <- plot + geom_blank()
  }

  # gganimate
  scene <- create_scene(plot$transition, plot$view, plot$shadow, plot$ease, plot$transmuters, plot$nframes)
  # --

  layers <- plot$layers
  data <- rep(list(NULL), length(layers))

  scales <- plot$scales

  # Allow all layers to make any final adjustments based
  # on raw input data and plot info
  data <- by_layer(function(l, d) l$layer_data(plot$data), layers, data, "computing layer data")
  data <- by_layer(function(l, d) l$setup_layer(d, plot), layers, data, "setting up layer")

  # gganimate
  # Extract scale names and merge it with label list
  scale_labels <- lapply(scales$scales, `[[`, 'name')
  names(scale_labels) <- vapply(scales$scales, function(sc) sc$aesthetics[1], character(1))
  lapply(scales$scales, function(sc) sc$name <- waiver())
  scale_labels <- scale_labels[!vapply(scale_labels, is.waive, logical(1))]

  # `setup_plot_labels()` is an internal function, but I'm sure the ggplot2
  # devs allow us some grace and leniency.
  setup_plot_labels <- get0("setup_plot_labels", asNamespace("ggplot2"))
  if (is.function(setup_plot_labels)) {
    plot$labels <- setup_plot_labels(plot, layers, data)
  }
  plot$labels[names(scale_labels)] <- scale_labels
  # --

  # Initialise panels, add extra data for margins & missing faceting
  # variables, and add on a PANEL variable to data
  layout <- create_layout(plot$facet, plot$coordinates)
  data <- layout$setup(data, plot$data, plot$plot_env)

  # gganimate
  scene$setup(data)
  # --

  # Compute aesthetics to produce data with generalised variable names
  data <- by_layer(function(l, d) l$compute_aesthetics(d, plot), layers, data, "computing aesthetics")

  # gganimate
  scene$identify_layers(data, layers)
  # --

  # Transform all scales
  data <- lapply(data, scales$transform_df)

  # Map and train positions so that statistics have access to ranges
  # and all positions are numeric
  scale_x <- function() scales$get_scales("x")
  scale_y <- function() scales$get_scales("y")

  layout$train_position(data, scale_x(), scale_y())
  data <- layout$map_position(data)

  # gganimate
  data <- scene$before_stat(data)
  # --

  # Apply and map statistics
  data <- by_layer(function(l, d) l$compute_statistic(d, layout), layers, data, "computing stat")
  data <- by_layer(function(l, d) l$map_statistic(d, plot), layers, data, "mapping stat to aesthetics")

  # gganimate
  data <- scene$after_stat(data)
  # --

  # Make sure missing (but required) aesthetics are added
  plot$scales$add_missing(c("x", "y"), plot$plot_env)

  # Reparameterise geoms from (e.g.) y and width to ymin and ymax
  data <- by_layer(function(l, d) l$compute_geom_1(d), layers, data, "setting up geom")

  # gganimate
  data <- scene$before_position(data)
  # --

  # Apply position adjustments
  data <- by_layer(function(l, d) l$compute_position(d, layout), layers, data, "computing position")

  # gganimate
  data <- scene$after_position(data)
  # --

  # Reset position scales, then re-train and map.  This ensures that facets
  # have control over the range of a plot: is it generated from what is
  # displayed, or does it include the range of underlying data
  layout$reset_scales()
  layout$train_position(data, scale_x(), scale_y())
  layout$setup_panel_params()
  data <- layout$map_position(data)

  layout$setup_panel_guides(plot$guides, plot$layers)

  complete_theme <- get0("complete_theme", asNamespace("ggplot2"))
  if (is.function(complete_theme)) {
    plot$theme <- complete_theme(plot$theme)
  }

  # Train and map non-position scales
  npscales <- scales$non_position_scales()
  if (npscales$n() > 0) {
    if (is.function(npscales$set_palettes)) {
      npscales$set_palettes(plot$theme)
      lapply(data, npscales$train_df)
      plot$guides <- plot$guides$build(npscales, plot$layers, plot$labels, data, theme = plot$theme)
    } else {
      lapply(data, npscales$train_df)
      plot$guides <- plot$guides$build(npscales, plot$layers, plot$labels, data)
    }
    data <- lapply(data, npscales$map_df)
  }

  # Fill in defaults etc.
  if (is.function(complete_theme)) {
    data <- by_layer(
      function(l, d) l$compute_geom_2(d, theme = plot$theme),
      layers, data, "setting up geom aesthetics"
    )
  } else {
    data <- by_layer(function(l, d) l$compute_geom_2(d), layers, data, "setting up geom aesthetics")
  }

  # gganimate
  data <- scene$after_defaults(data)
  # --

  # Let layer stat have a final say before rendering
  data <- by_layer(function(l, d) l$finish_statistics(d), layers, data, "finishing layer stat")

  # Let Layout modify data before rendering
  data <- layout$finish_data(data)

  # gganimate
  data <- scene$finish_data(data)
  # --

  # Consolidate alt-text
  plot$labels$alt <- plot$labels[["alt"]] %||% ""

  structure(
    list(data = data, layout = layout, plot = plot, scene = scene),
    class = "gganim_built"
  )
}

# Apply function to layer and matching data
by_layer <- function(f, layers, data, step = NULL) {
  ordinal <- scales::label_ordinal()
  out <- vector("list", length(data))
  try_fetch(
    for (i in seq_along(data)) {
      out[[i]] <- f(l = layers[[i]], d = data[[i]])
    },
    error = function(cnd) {
      cli::cli_abort(c("Problem while {step}.", "i" = "Error occurred in the {ordinal(i)} layer."), call = layers[[i]]$constructor, parent = cnd)
    }
  )
  out
}
