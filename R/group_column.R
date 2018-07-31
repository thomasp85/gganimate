#' @rdname layer_type
#'
#' @export
group_column <- function(x) {
  UseMethod('group_column')
}
#' @export
group_column.default <- function(x) NULL
