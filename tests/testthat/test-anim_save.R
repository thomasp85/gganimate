context("test-anim_save")

test_that("anim_save overwrites existing files", {

  skip_on_os('solaris') # gifski not available
  skip_if_not_installed('gifski')
  skip_if_not_installed('transformr')

  p <- ggplot(airquality, aes(Day, Temp)) +
    geom_line(color = 'red') +
    transition_time(Month)

  capture.output({
    a1 <- animate(p, nframes = 2)
    a2 <- animate(p, nframes = 3)
  })

  tf <- tempfile()
  on.exit(unlink(tf))

  anim_save(a1, file = tf)

  frames_1 <- nrow(magick::image_info(magick::image_read(tf)))
  expect_equal(frames_1, 2)

  anim_save(a2, file = tf)

  frames_2 <- nrow(magick::image_info(magick::image_read(tf)))

  expect_equal(frames_2, 3)
})
