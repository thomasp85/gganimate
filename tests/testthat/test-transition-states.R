context("transition-states")

test_that("area works", {
  skip_if_not_installed('transformr')
  set.seed(1)
  df <- data.frame(
    x = c(1:10, 1:10),
    y = runif(20),
    f = rep(c('a', 'b'), each = 10)
  )
  p <- ggplot(df) + geom_area(aes(x, y)) + transition_states(f, 2, 1)
  expect_silent(prerender(p, 50))
})

test_that('bar works', {
  set.seed(1)
  df <- data.frame(
    x = sample(letters[1:5], 20, replace = TRUE),
    f = rep(c('a', 'b'), each = 10)
  )
  p <- ggplot(df) + geom_bar(aes(x)) + transition_states(f, 2, 1)
  expect_silent(prerender(p, 50))
})

test_that('bin2d works', {
  set.seed(1)
  df <- data.frame(
    x = c(rnorm(100), runif(100)),
    y = c(rnorm(100), runif(100)),
    f = rep(c('a', 'b'), each = 100)
  )
  p <- ggplot(df) + geom_bin2d(aes(x, y), drop = FALSE, bins = 30) + transition_states(f, 2, 1)
  expect_silent(prerender(p, 50))
})

test_that('boxplot works', {
  p <- ggplot(mtcars) + geom_boxplot(aes(factor(cyl), mpg)) + transition_states(gear, 2, 1)
  expect_silent(prerender(p, 50))
})

test_that('col works', {
  df <- data.frame(
    x = c(1:5, 1:5),
    y = c(1:5, 5:1),
    f = rep(c('a', 'b'), each = 5)
  )
  p <- ggplot(df) + geom_col(aes(x, y)) + transition_states(f, 2, 1)
  expect_silent(prerender(p, 50))
})

test_that('contour works', {
  # Investigate why this fails on some architectures
  skip_on_cran()
  df <- data.frame(
    x = rep(rep(1:10, each = 10), 2),
    y = rep(rep(1:10, 10), 2),
    z = c(volcano[1:10, 1:10], volcano[21:30, 21:30]),
    f = rep(c('a', 'b'), each = 100)
  )
  p <- ggplot(df) + geom_contour(aes(x, y, z = z)) + transition_states(f, 2, 1)
  expect_silent(prerender(p, 50))
})
