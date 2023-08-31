transform_path <- function(...) {
  check_installed('transformr', 'to tween paths and lines')
  transformr::tween_path(...)
}
transform_polygon <- function(...) {
  check_installed('transformr', 'to tween polygons')
  transformr::tween_polygon(...)
}
transform_sf <- function(...) {
  check_installed('transformr', 'to tween sf layers')
  transformr::tween_sf(...)
}
