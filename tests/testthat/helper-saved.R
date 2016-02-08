check_image_file <- function(f, min_size = NULL) {
  info <- file.info(f)
  expect_true(!is.na(info$size))
  expect_gt(info$size, 0)

  if (!is.null(min_size)) {
    expect_gte(info$size, min_size)
  }
}
