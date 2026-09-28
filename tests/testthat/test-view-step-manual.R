# Helper: create a 2-window ViewStepManual object and run setup_params
step_manual_params <- function(wrap, pause_first) {
  vsm <- view_step_manual(
    wrap = wrap, pause_first = pause_first,
    xmin = c(1, 4), xmax = c(7, 7), ymin = 0, ymax = 2.5
  )
  vsm$setup_params(NULL, vsm$params)
}

# --- Phase structure (setup_params) ---
# Each phase is a (pause, step) pair. pause_length and step_length should
# reflect the minimum number of phases needed for the given wrap/pause_first.

test_that("wrap=FALSE pause_first=FALSE produces 2 phases: transition then pause", {
  p <- step_manual_params(wrap = FALSE, pause_first = FALSE)
  expect_equal(p$pause_length, c(0, 1))
  expect_equal(p$step_length,  c(1, 0))
})

test_that("wrap=FALSE pause_first=TRUE produces 2 phases: pause-transition then pause", {
  p <- step_manual_params(wrap = FALSE, pause_first = TRUE)
  expect_equal(p$pause_length, c(1, 1))
  expect_equal(p$step_length,  c(1, 0))
})

test_that("wrap=TRUE pause_first=FALSE produces 2 phases: transition then pause+transition", {
  p <- step_manual_params(wrap = TRUE, pause_first = FALSE)
  expect_equal(p$pause_length, c(0, 1))
  expect_equal(p$step_length,  c(1, 1))
})

test_that("wrap=TRUE pause_first=TRUE produces 2 phases: both with transitions for wrap", {
  p <- step_manual_params(wrap = TRUE, pause_first = TRUE)
  expect_equal(p$pause_length, c(1, 1))
  expect_equal(p$step_length,  c(1, 1))
})

# --- Frame allocation (distribute_frames) ---
# nframes chosen to divide cleanly across phases (no rounding artefacts).

test_that("wrap=FALSE pause_first=FALSE: frames split equally between transition and pause", {
  # total weight = 2, nframes = 100 -> 50 frames each
  p <- step_manual_params(wrap = FALSE, pause_first = FALSE)
  frames <- gganimate:::distribute_frames(p$pause_length, p$step_length, 100)
  expect_equal(frames$static_length,     c(0,  50))
  expect_equal(frames$transition_length, c(50,  0))
})

test_that("wrap=FALSE pause_first=TRUE: frames split equally across pause, transition, pause", {
  # total weight = 3, nframes = 99 -> 33 frames each
  p <- step_manual_params(wrap = FALSE, pause_first = TRUE)
  frames <- gganimate:::distribute_frames(p$pause_length, p$step_length, 99)
  expect_equal(frames$static_length,     c(33, 33))
  expect_equal(frames$transition_length, c(33,  0))
})

test_that("wrap=TRUE pause_first=FALSE: frames split equally across transition, pause, transition", {
  # total weight = 3, nframes = 99 -> 33 frames each; no trailing w1 pause
  p <- step_manual_params(wrap = TRUE, pause_first = FALSE)
  frames <- gganimate:::distribute_frames(p$pause_length, p$step_length, 99)
  expect_equal(frames$static_length,     c( 0, 33))
  expect_equal(frames$transition_length, c(33, 33))
})

test_that("wrap=TRUE pause_first=TRUE: frames split equally across pause, transition, pause, transition", {
  # total weight = 4, nframes = 100 -> 25 frames each
  p <- step_manual_params(wrap = TRUE, pause_first = TRUE)
  frames <- gganimate:::distribute_frames(p$pause_length, p$step_length, 100)
  expect_equal(frames$static_length,     c(25, 25))
  expect_equal(frames$transition_length, c(25, 25))
})
