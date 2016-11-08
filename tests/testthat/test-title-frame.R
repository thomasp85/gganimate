context('`title_frame` argument')

library(ggplot2)

extract_titles <- function(anim) {
  vapply(anim$plots, function(p) p$plot$labels$title, character(1))
}

test_that('formula must be one-sided', {
  p <- ggplot(iris, aes(Sepal.Length, Sepal.Width, frame = Species)) +
    geom_point()

  expect_error(a <- gg_animate(p, title_frame = x ~ round(., 4)), 'one-sided')
})

test_that('default concatenation', {
  mthammer <- mtcars
  mthammer$name <- rownames(mtcars)
  p <- ggplot(mthammer, aes(mpg, disp, frame = name)) +
    geom_point()

  a <- gg_animate(p)
  mttitle <- sort(unique(mthammer$name))
  expect_equal(extract_titles(a), mttitle)

  p_with_title <- p + labs(title = 'Make & Model:')
  a2 <- gg_animate(p_with_title)
  mtconcat <- paste('Make & Model:', sort(unique(mthammer$name)))
  expect_equal(extract_titles(a2), mtconcat)
})

test_that('`.title` refers to plot title', {
  ptitle <- 'Depths Across Magnitudes'
  p <- ggplot(quakes, aes(lat, long, frame = mag)) +
    geom_point() +
    labs(title = ptitle)
  a <- gg_animate(p, title_frame = ~ .title)

  expect_true(all(extract_titles(a) == ptitle))

  p2 <- p + labs(title = 'Depths/Mag')
  a2 <- gg_animate(p2, title_frame = ~ paste('~~~', .title, '~~~'))
  expect_true(all(extract_titles(a2) == '~~~ Depths/Mag ~~~'))
})

test_that('`.` refers to frame value', {
  robinson <- swiss
  robinson$district <- rownames(swiss)
  p <- ggplot(robinson, aes(Education, Catholic, frame = district)) +
    geom_point(aes(cumulative = TRUE))
  a <- gg_animate(p, title_frame = ~ paste('District', .))
  dists <- sort(unique(robinson$district))
  expect_equal(extract_titles(a), paste('District', dists))
})
