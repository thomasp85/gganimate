context("ggplot alignment")

# Helper: parse y-coordinates of text elements from an SVG file.
# Returns a named list with entries for elements matching each selector pattern.
svg_text_y <- function(svg_path) {
  xml <- xml2::read_xml(svg_path)
  xml2::xml_ns_strip(xml)
  nodes <- xml2::xml_find_all(xml, "//text")
  y_vals <- as.numeric(xml2::xml_attr(nodes, "y"))
  y_vals[!is.na(y_vals)]
}

test_that("animate and ggsave produce the same layout with svg device", {
  skip_if_not_installed("xml2")
  skip_if_not_installed("svglite")

  df <- data.frame(
    x = rep(1:5, 2),
    y = c(1:5, 5:1),
    state = rep(1:2, each = 5)
  )

  base_plot <- ggplot(df, aes(x, y)) +
    labs(title = "Title", subtitle = "Subtitle", caption = "Caption") +
    theme_minimal(base_size = 14) +
    scale_x_continuous(limits = c(0, 6)) +
    scale_y_continuous(limits = c(0, 6))

  # ggsave reference: render state 1 directly
  ggplot_svg <- tempfile(fileext = ".svg")
  ggplot2::ggsave(
    plot = ggplot(subset(df, state == 1), aes(x, y)) +
      geom_point() +
      labs(title = "Title", subtitle = "Subtitle", caption = "Caption") +
      theme_minimal(base_size = 14) +
      scale_x_continuous(limits = c(0, 6)) +
      scale_y_continuous(limits = c(0, 6)),
    filename = ggplot_svg,
    device = svglite::svglite,
    width = 5, height = 5
  )

  # animate: render state 1 frame via gganimate with svglite device
  anim <- base_plot +
    geom_point() +
    transition_states(state, wrap = FALSE)

  frame_paths <- animate(
    anim,
    renderer = function(frames, fps) frames,
    nframes = 2,
    fps = 1,
    device = "svglite",
    width = 5,
    height = 5,
    units = "in"
  )

  gganimate_svg <- frame_paths[[1]]

  ggplot_y  <- svg_text_y(ggplot_svg)
  gganimate_y <- svg_text_y(gganimate_svg)

  # Both renders should produce the same number of text elements at the same
  # y-positions (within floating-point tolerance).
  expect_equal(length(ggplot_y), length(gganimate_y))
  expect_equal(ggplot_y, gganimate_y, tolerance = 0.5)
})
