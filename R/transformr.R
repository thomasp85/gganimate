transform_path <- function(...) {
  if (!requireNamespace('transformr', quietly = TRUE)) {
    stop('The transformr package is required to tween paths and lines')
  }
  transformr::tween_path(...)
}
transform_polygon <- function(...) {
  if (!requireNamespace('transformr', quietly = TRUE)) {
    stop('The transformr package is required to tween polygons')
  }
  transformr::tween_polygon(...)
}
transform_sf <- function(...) {
  if (!requireNamespace('transformr', quietly = TRUE)) {
    stop('The transformr package is required to tween sf layers')
  }
  transformr::tween_sf(...)
}
