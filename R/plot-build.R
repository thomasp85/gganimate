#' @importFrom ggplot2 ggplot_build geom_blank waiver
#' @export
ggplot_build.gganim <- function(plot) {
  plot <- plot_clone(plot)
  if (length(plot$layers) == 0) {
    plot <- plot + geom_blank()
  }
  scene <- create_scene(plot$transition, plot$view, plot$shadow, plot$ease, plot$transmuters, plot$nframes)
  layers <- plot$layers
  layer_data <- lapply(layers, function(y) y$layer_data(plot$data))

  scales <- plot$scales

  # Extract scale names and merge it with label list
  scale_labels <- lapply(scales$scales, `[[`, 'name')
  names(scale_labels) <- vapply(scales$scales, function(sc) sc$aesthetics[1], character(1))
  lapply(scales$scales, function(sc) sc$name <- waiver())
  scale_labels <- scale_labels[!vapply(scale_labels, is.waive, logical(1))]
  plot$labels[names(scale_labels)] <- scale_labels

  # Apply function to layer and matching data
  by_layer <- function(f) {
    out <- vector("list", length(data))
    for (i in seq_along(data)) {
      out[[i]] <- f(l = layers[[i]], d = data[[i]])
    }
    out
  }

  # Initialise panels, add extra data for margins & missing faceting
  # variables, and add on a PANEL variable to data

  layout <- create_layout(plot$facet, plot$coordinates)
  data <- layout$setup(layer_data, plot$data, plot$plot_env)
  scene$setup(data)
  # Compute aesthetics to produce data with generalised variable names
  data <- by_layer(function(l, d) l$compute_aesthetics(d, plot))

  scene$identify_layers(data, layers)
  # Transform all scales
  data <- lapply(data, scales_transform_df, scales = scales)

  # Map and train positions so that statistics have access to ranges
  # and all positions are numeric
  scale_x <- function() scales$get_scales("x")
  scale_y <- function() scales$get_scales("y")

  layout$train_position(data, scale_x(), scale_y())
  data <- layout$map_position(data)

  data <- scene$before_stat(data)

  # Apply and map statistics
  data <- by_layer(function(l, d) l$compute_statistic(d, layout))
  data <- by_layer(function(l, d) l$map_statistic(d, plot))

  data <- scene$after_stat(data)

  # Make sure missing (but required) aesthetics are added
  scales_add_missing(plot, c("x", "y"), plot$plot_env)

  # Reparameterise geoms from (e.g.) y and width to ymin and ymax
  data <- by_layer(function(l, d) l$compute_geom_1(d))

  data <- scene$before_position(data)

  # Apply position adjustments
  data <- by_layer(function(l, d) l$compute_position(d, layout))

  data <- scene$after_position(data)

  # Reset position scales, then re-train and map.  This ensures that facets
  # have control over the range of a plot: is it generated from what is
  # displayed, or does it include the range of underlying data
  layout$reset_scales()
  layout$train_position(data, scale_x(), scale_y())
  layout$setup_panel_params()
  data <- layout$map_position(data)

  # Train and map non-position scales
  npscales <- scales$non_position_scales()
  if (npscales$n() > 0) {
    lapply(data, scales_train_df, scales = npscales)
    data <- lapply(data, scales_map_df, scales = npscales)
  }

  # Fill in defaults etc.
  data <- by_layer(function(l, d) l$compute_geom_2(d))

  data <- scene$after_defaults(data)

  # Let layer stat have a final say before rendering
  data <- by_layer(function(l, d) l$finish_statistics(d))

  # Let Layout modify data before rendering
  data <- layout$finish_data(data)

  data <- scene$finish_data(data)

  structure(
    list(data = data, layout = layout, plot = plot, scene = scene),
    class = "gganim_built"
  )
}
