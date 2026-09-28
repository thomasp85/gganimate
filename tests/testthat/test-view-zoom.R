zoom_params <- function(
  wrap,
  pause_first,
  look_ahead = 0,
  nsteps = 2,
  pause_length = 1,
  step_length = 1
) {
  vz <- view_zoom(
    wrap = wrap,
    pause_first = pause_first,
    pause_length = pause_length,
    step_length = step_length,
    look_ahead = look_ahead,
    nsteps = nsteps
  )
  vz$setup_params(NULL, vz$params)
}

zoom_train_params <- function(
  wrap,
  pause_first,
  look_ahead = 0,
  nsteps = 2,
  nframes = 100,
  pause_length = 1,
  step_length = 1
) {
  params <- zoom_params(
    wrap = wrap,
    pause_first = pause_first,
    look_ahead = look_ahead,
    nsteps = nsteps,
    pause_length = pause_length,
    step_length = step_length
  )
  params$nframes <- nframes
  params$excluded_layers <- integer()

  # Data whose range varies monotonically and distinguishably with frame
  # index, so a collapsed or drifting set of view windows is detectable from
  # the resulting frame_ranges rather than being masked by coincidence.
  layer_data <- lapply(seq_len(nframes), function(i) {
    data_frame0(x = c(i, i + 1), y = c(i, i + 1))
  })

  ViewZoom$train(list(layer_data), params)
}

# setup_params() phase construction ------------------------------------------
# ViewZoom inherits ViewStep's setup_params() and train() unchanged - only
# window_transition() (the pan/zoom trajectory) is its own. These mirror
# test-view-step.R's phase-structure tests to guard the shared code from
# this side too.

test_that("wrap=FALSE pause_first=FALSE produces 3 phases with the closing step zeroed", {
  p <- zoom_params(wrap = FALSE, pause_first = FALSE)
  expect_equal(p$pause_length, c(0, 1, 1))
  expect_equal(p$step_length, c(1, 0, 0))
})

test_that("wrap=FALSE pause_first=TRUE produces 2 phases", {
  p <- zoom_params(wrap = FALSE, pause_first = TRUE)
  expect_equal(p$pause_length, c(1, 1))
  expect_equal(p$step_length, c(1, 0))
})

test_that("wrap=TRUE pause_first=FALSE produces 3 phases", {
  p <- zoom_params(wrap = TRUE, pause_first = FALSE)
  expect_equal(p$pause_length, c(0, 1, 1))
  expect_equal(p$step_length, c(1, 1, 0))
})

test_that("wrap=TRUE pause_first=TRUE produces 2 phases", {
  p <- zoom_params(wrap = TRUE, pause_first = TRUE)
  expect_equal(p$pause_length, c(1, 1))
  expect_equal(p$step_length, c(1, 1))
})

# train() frame-id bookkeeping -------------------------------------------------

test_that("train assigns frame ids within 0..nframes-1 for non-wrapping views", {
  p <- zoom_train_params(wrap = FALSE, pause_first = FALSE, nframes = 12)
  expect_equal(sort(unique(p$frame_ranges$.frame)), 0:11)
  expect_equal(nrow(p$frame_ranges), 12)
})

test_that("train assigns frame ids within 0..nframes-1 for wrapping views", {
  p <- zoom_train_params(wrap = TRUE, pause_first = FALSE, nframes = 12)
  expect_equal(sort(unique(p$frame_ranges$.frame)), 0:11)
  expect_equal(nrow(p$frame_ranges), 12)
})

# train() end-to-end window/range regression -----------------------------------
# Guards against the same #499 failure mode as view_step: a closing
# transition that never actually stops moving. window_transition() is
# ViewZoom's own pan/zoom trajectory, not simple tweening, so this also
# checks that trajectory correctly comes to rest at its target window.

test_that("wrap=FALSE view holds exactly static at the true final state, not just frozen somewhere", {
  # include = FALSE avoids ambiguity from neighbour-window merging, so the
  # final held view is traceable directly to the last frame's own data range:
  # layer_data for frame i is x = c(i, i + 1), so frame 100 -> (100, 101).
  vz <- view_zoom(
    wrap = FALSE,
    pause_first = FALSE,
    pause_length = 2,
    step_length = 1,
    look_ahead = 2,
    nsteps = 3,
    include = FALSE
  )
  params <- vz$setup_params(NULL, vz$params)
  params$nframes <- 100
  params$excluded_layers <- integer()
  layer_data <- lapply(seq_len(100), function(i) {
    data_frame0(x = c(i, i + 1), y = c(i, i + 1))
  })
  p <- ViewZoom$train(list(layer_data), params)

  fr <- p$frame_ranges[order(p$frame_ranges$.frame), ]
  tail_fr <- utils::tail(fr, 10)
  expect_equal(unique(tail_fr$xmin), 100)
  expect_equal(unique(tail_fr$xmax), 101)
})
