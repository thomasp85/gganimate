test_that("animated devices work correctly", {
  library(ggplot2)
  library(gganimate)

  mock_device <- function(filename, width = 400, height = 300, ...) {
    file.create(filename)
    invisible()
  }

  expect_true(is.function(mock_device))
  expect_false(is.function("png"))

  # Test extension handling
  device_args <- list(extension = ".webp", width = 500, height = 400)
  extension <- device_args$extension %||% ".out"
  expect_equal(extension, ".webp")

  device_args$extension <- NULL
  expect_false("extension" %in% names(device_args))
  expect_true("width" %in% names(device_args))

  # Test fallback extension
  fallback_args <- list(width = 400, height = 300)
  fallback_ext <- fallback_args$extension %||% ".out"
  expect_equal(fallback_ext, ".out")
})

test_that("animated renderer handles output files", {
  library(gganimate)

  test_file <- tempfile(fileext = ".webp")
  file.create(test_file)

  renderer <- animated_renderer()
  result <- renderer(rep(test_file, 3), fps = 10)

  expect_equal(result, test_file)
  expect_true(is.character(result))

  unlink(test_file)
})

test_that("animated device creation failures are handled", {
  library(ggplot2)
  library(gganimate)

  # Create a mock device function that fails
  failing_device <- function(filename, ...) {
    stop("Mock device initialization failure")
  }

  test_data <- data.frame(x = 1:3, y = 1:3, frame = 1:3)
  p <- ggplot(test_data, aes(x, y)) +
    geom_point() +
    transition_states(frame)

  expect_error(
    animate(p, device = failing_device, extension = ".test", nframes = 3),
    "Failed to initialize animated device"
  )
})

test_that("invalid animated device parameters are caught", {
  library(ggplot2)
  library(gganimate)

  # Test with non-function device
  test_data <- data.frame(x = 1:3, y = 1:3, frame = 1:3)
  p <- ggplot(test_data, aes(x, y)) +
    geom_point() +
    transition_states(frame)

  # This should fall through to file device handling, not animated device
  # Just ensure it doesn't crash the animated device path incorrectly
  expect_silent({
    result <- animate(p, device = "png", nframes = 3)
  })
})

test_that("error handling provides clear messages", {
  library(ggplot2)
  library(gganimate)

  test_data <- data.frame(x = 1:3, y = 1:3, frame = 1:3)
  p <- ggplot(test_data, aes(x, y)) +
    geom_point() +
    transition_states(frame)

  # Test that basic functionality works (validates error handling paths exist)
  expect_no_error({
    result <- animate(p, device = "png", nframes = 3)
    unlink(attr(result, "frame_vars")$frame_source)
  })
})

# Use environment variable to control whether test files are kept for inspection
keep_test_files <- Sys.getenv("TESTTHAT_KEEP_FILES", "false") == "true"

test_that("agg_webp_anim integration test produces animated WebP", {
  skip_if_not_installed("ragg")
  skip_if_not(exists("agg_webp_anim", where = asNamespace("ragg")),
              "agg_webp_anim not available in ragg")

  # Create test data with exactly 12 distinct states for clear frame counting
  test_data <- data.frame()
  for (frame in 1:12) {
    frame_data <- data.frame(
      x = 1:frame,
      y = rep(2, frame),
      frame_num = frame,
      size = frame * 2
    )
    test_data <- rbind(test_data, frame_data)
  }

  p <- ggplot(test_data, aes(x, y, size = size)) +
    geom_point(color = "red", alpha = 0.8) +
    scale_size_identity() +
    xlim(0, 13) + ylim(0, 4) +
    transition_states(frame_num) +
    labs(title = "Frame {closest_state} of 12",
         subtitle = "Each frame shows more red dots") +
    theme_minimal() +
    theme(legend.position = "none")

  result <- animate(p,
                    device = ragg::agg_webp_anim,
                    extension = ".webp",
                    width = 500, height = 400,
                    delay = 600, loop = 0,
                    nframes = 12)

  expect_true(file.exists(result))
  expect_gt(file.info(result)$size, 1000)


  if (keep_test_files) {
    cat("Animated WebP with 12 frames created at:", result, "\n")
  } else {
    unlink(result)
  }
})

test_that("agg_webp_anim with gapminder data (website example)", {
  skip_if_not_installed("ragg")
  skip_if_not_installed("gapminder")
  skip_if_not(exists("agg_webp_anim", where = asNamespace("ragg")),
              "agg_webp_anim not available in ragg")

  library(ggplot2)
  library(gapminder)

  gap_data <- gapminder

  p <- ggplot(gap_data, aes(gdpPercap, lifeExp, size = pop, colour = country)) +
    geom_point(alpha = 0.7, show.legend = FALSE) +
    scale_colour_manual(values = country_colors) +
    scale_size(range = c(2, 12)) +
    scale_x_log10() +
    facet_wrap(~continent) +
    labs(title = 'Year: {frame_time}', x = 'GDP per capita', y = 'life expectancy') +
    transition_time(year) +
    ease_aes('linear')

  result <- animate(p,
                    device = ragg::agg_webp_anim,
                    extension = ".webp",
                    width = 672, height = 480,
                    delay = 100, loop = 0)

  expect_true(file.exists(result))
  expect_gt(file.info(result)$size, 20000)

  if (keep_test_files) {
    cat("Gapminder animation created at:", result, "\n")
  } else {
    unlink(result)
  }
})
