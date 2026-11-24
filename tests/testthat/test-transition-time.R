test_that("transition_time clips the requested time range after tweening", {
  data <- data.frame(x = 1:3, y = 1, time = as.double(7:9))
  plot <- ggplot(data, aes(x, y)) +
    geom_point()
  frame_positions <- function(range) {
    built <- prerender(plot + transition_time(time, range = range), 7)
    vapply(seq_len(7), function(i) {
      built$scene$get_frame(built, i)$data[[1]]$x
    }, numeric(1))
  }

  expect_equal(frame_positions(c(8, 9)), seq(2, 3, length.out = 7))
  expect_equal(frame_positions(c(7.5, 8.5)), seq(1.5, 2.5, length.out = 7))
  expect_equal(frame_positions(NULL), seq(1, 3, length.out = 7))
})

test_that("transition_time omits panels outside the requested time range", {
  visible <- data.frame(x = 1:3, y = 1, time = as.double(7:9), panel = "visible")
  before <- transform(visible, x = x + 10, time = time - 6, panel = "before")
  after <- transform(visible, x = x + 20, time = time + 4, panel = "after")
  plot <- ggplot(rbind(before, visible, after), aes(x, y)) +
    geom_point() +
    facet_wrap(~panel) +
    transition_time(time, range = c(8, 9))
  built <- prerender(plot, 7)

  expect_equal(built$scene$get_frame(built, 1)$data[[1]]$x, 2)
  expect_equal(built$scene$get_frame(built, 7)$data[[1]]$x, 3)
})
