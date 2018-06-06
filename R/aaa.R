#' @importFrom ggplot2 scale_x_continuous
x_aes <- scale_x_continuous()$aesthetics
#' @importFrom ggplot2 scale_y_continuous
y_aes <- scale_y_continuous()$aesthetics

#' @importFrom rlang eval_tidy
safe_eval <- function(expr, data) {
  value <- eval_tidy(expr, data)
  if (!is.function(value) &&
      !is.environment(value) &&
      !is.symbol(value) &&
      !is.language(value) &&
      (length(value) == 1 || length(value) == nrow(data))) {
    value
  } else {
    NULL
  }
}
