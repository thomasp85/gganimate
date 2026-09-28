step_params <- function(
  wrap,
  pause_first,
  look_ahead = 1,
  nsteps = 2,
  pause_length = 1,
  step_length = 1
) {
  vs <- view_step(
    wrap = wrap,
    pause_first = pause_first,
    pause_length = pause_length,
    step_length = step_length,
    look_ahead = look_ahead,
    nsteps = nsteps
  )
  vs$setup_params(NULL, vs$params)
}

step_train_params <- function(
  wrap,
  pause_first,
  look_ahead = 1,
  nsteps = 2,
  nframes = 100,
  pause_length = 1,
  step_length = 1
) {
  params <- step_params(
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

  ViewStep$train(list(layer_data), params)
}

# setup_params() phase construction ------------------------------------------
# For pause_first = FALSE, the phase vectors get one extra leading/trailing
# slot (a leading zero-length pause, a trailing zero-length step) so that the
# view starts on a step and the final phase can double as the wrap-around
# closing segment. That extra slot is load-bearing for train()'s break/window
# selection when wrap = TRUE (see below) - it must stay the same length as
# pause_first = TRUE's un-extended vectors would imply, i.e. nsteps + 1.

test_that("wrap=FALSE pause_first=FALSE produces 3 phases with the closing step zeroed", {
  p <- step_params(wrap = FALSE, pause_first = FALSE)
  expect_equal(p$pause_length, c(0, 1, 1))
  expect_equal(p$step_length, c(1, 0, 0))
})

test_that("wrap=FALSE pause_first=TRUE produces 2 phases", {
  p <- step_params(wrap = FALSE, pause_first = TRUE)
  expect_equal(p$pause_length, c(1, 1))
  expect_equal(p$step_length, c(1, 0))
})

test_that("wrap=TRUE pause_first=FALSE produces 3 phases", {
  p <- step_params(wrap = TRUE, pause_first = FALSE)
  expect_equal(p$pause_length, c(0, 1, 1))
  expect_equal(p$step_length, c(1, 1, 0))
})

test_that("wrap=TRUE pause_first=TRUE produces 2 phases", {
  p <- step_params(wrap = TRUE, pause_first = TRUE)
  expect_equal(p$pause_length, c(1, 1))
  expect_equal(p$step_length, c(1, 1))
})

test_that("look_ahead is recycled to nsteps (+1 extra phase for pause_first=FALSE)", {
  p <- step_params(
    wrap = TRUE,
    pause_first = FALSE,
    look_ahead = c(1, 2, 3),
    nsteps = 5
  )
  expect_equal(p$look_ahead, c(1, 2, 3, 1, 2, 1))
})

# train() frame-id bookkeeping -------------------------------------------------

test_that("train assigns frame ids within 0..nframes-1 for non-wrapping views", {
  p <- step_train_params(wrap = FALSE, pause_first = FALSE, nframes = 12)
  expect_equal(sort(unique(p$frame_ranges$.frame)), 0:11)
  expect_equal(nrow(p$frame_ranges), 12)
})

test_that("train assigns frame ids within 0..nframes-1 for wrapping views", {
  p <- step_train_params(wrap = TRUE, pause_first = FALSE, nframes = 12)
  expect_equal(sort(unique(p$frame_ranges$.frame)), 0:11)
  expect_equal(nrow(p$frame_ranges), 12)
})

# train() end-to-end window/range regressions ---------------------------------
# These guard against the actual reported bug (wrap=FALSE drifting back at the end)

test_that("wrap=FALSE view holds exactly static at the true final state, not just frozen somewhere", {
  # include = FALSE avoids ambiguity from neighbour-window merging, so the
  # final held view is traceable directly to the last frame's own data range:
  # layer_data for frame i is x = c(i, i + 1), so frame 100 -> (100, 101).
  vs <- view_step(
    wrap = FALSE,
    pause_first = FALSE,
    pause_length = 2,
    step_length = 1,
    look_ahead = 2,
    nsteps = 3,
    include = FALSE
  )
  params <- vs$setup_params(NULL, vs$params)
  params$nframes <- 100
  params$excluded_layers <- integer()
  layer_data <- lapply(seq_len(100), function(i) {
    data_frame0(x = c(i, i + 1), y = c(i, i + 1))
  })
  p <- ViewStep$train(list(layer_data), params)

  fr <- p$frame_ranges[order(p$frame_ranges$.frame), ]
  tail_fr <- utils::tail(fr, 10)
  # Issue #499: the closing transition wasn't actually suppressed, so the
  # last frames drifted back toward the first window (and never settled
  # exactly on the last state's true range) instead of holding.
  expect_equal(unique(tail_fr$xmin), 100)
  expect_equal(unique(tail_fr$xmax), 101)
})
