#' @importFrom ggplot2 scale_x_continuous
x_aes <- scale_x_continuous()$aesthetics
#' @importFrom ggplot2 scale_y_continuous
y_aes <- scale_y_continuous()$aesthetics

#' @importFrom rlang eval_tidy
safe_eval <- function(expr, data) {
  value <- try_fetch(
    eval_tidy(expr, data),
    error = function(e) numeric()
  )
  if (!is_function(value) &&
      !is_environment(value) &&
      !is_symbol(value) &&
      !is.language(value) &&
      (length(value) == 1 || length(value) == nrow(data))) {
    value
  } else {
    NULL
  }
}
#' @importFrom rlang quo_is_missing
require_quo <- function(expr, name) {
  if (quo_is_missing(expr)) {
    cli::cli_abort('{.arg {name}} must be provided')
  }
}
require_stat <- function(x) {
  if (is_call(x)) {
    if (identical(x[[1]], quote(stat)) || identical(x[[1]], quote(after_stat))) {
      TRUE
    } else {
      any(vapply(x, require_stat, logical(1)))
    }
  } else {
    FALSE
  }
}
`%?%` <- function(l, r) if (missing(l)) r else l

png_dim <- function(file) {
  if (!file.exists(file)) {
    cli::cli_abort('Provided file ({file}) does not exist')
  }
  bts <- as.integer(readBin(file, n = 24L, what = "raw"))
  if (!all(bts[1:8] == c(137, 80, 78, 71, 13, 10, 26 ,10))) {
    cli::cli_abort('Provided file ({file}) does not appear to be a png')
  }
  c(bts[21] * 2^24 + bts[22] * 2^16 + bts[23] * 2^8 + bts[24],
    bts[17] * 2^24 + bts[18] * 2^16 + bts[19] * 2^8 + bts[20])
}
