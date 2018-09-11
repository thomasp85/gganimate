#' @importFrom ggplot2 scale_x_continuous
x_aes <- scale_x_continuous()$aesthetics
#' @importFrom ggplot2 scale_y_continuous
y_aes <- scale_y_continuous()$aesthetics

#' @importFrom rlang eval_tidy
safe_eval <- function(expr, data) {
  value <- tryCatch(eval_tidy(expr, data), error = function(e) numeric())
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
#' @importFrom rlang quo_is_missing
require_quo <- function(expr, name) {
  if (quo_is_missing(expr)) {
    stop(name, ' must be provided', call. = FALSE)
  }
}
require_stat <- function(x) {
  if (is.call(x)) {
    if (identical(x[[1]], quote(stat))) {
      TRUE
    } else {
      any(vapply(x, require_stat, logical(1)))
    }
  } else {
    FALSE
  }
}
`%?%` <- function(l, r) if (missing(l)) r else l
