# create a small example plot
library(ggplot2)
p <- ggplot(mtcars, aes(wt, mpg, frame = cyl)) +
  geom_point()

context("Saving animations into files")

# test that we can create a GIF and that it exists
test_that("We can create an animated GIF", {
  s <- gg_animate_save(gg_animate(p))
  expect_is(s, "gg_animate")

  # check the file exists
  check_image_file(s$filename)
  # check that it has an src with the file loaded
  expect_is(s$src, "character")
  expect_match(s$src, "^data:image\\/gif;")
})


test_that("We can create an animated mp4 or avi", {
  for (ext in c("mp4", "avi")) {
    s <- gg_animate_save(gg_animate(p), saver = ext)
    expect_is(s, "gg_animate")

    # check the file exists
    check_image_file(s$filename)
    # check that it has an src with the file loaded
    expect_is(s$src, "character")
    expect_match(s$src, paste0("^data:video\\/", ext))
  }
})
