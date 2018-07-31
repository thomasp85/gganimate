context("transition-states")

test_that("area works", {
  set.seed(1)
  df <- data.frame(
    x = c(1:10, 1:10),
    y = runif(20),
    f = rep(c('a', 'b'), each = 10)
  )
  p <- ggplot(df) + geom_area(aes(x, y)) + transition_states(f, 2, 1)
  expect_silent(prerender(p, 50))
})

test_that('bar works' {
  set.seed(1)
  df <- data.frame(
    x = sample(letters[1:5], 20, replace = TRUE),
    f = rep(c('a', 'b'), each = 10)
  )
  p <- ggplot(df) + geom_bar(aes(x)) + transition_states(f, 2, 1)
  expect_silent(prerender(p, 50))
})
