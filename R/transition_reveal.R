#' Reveal data along a given dimension
#'
#' This transition allows you to let data gradually appear, based on a given
#' time dimension. In contrast to e.g. [transition_time()] `transition_reveal()`
#' calculates intermediary values at exact positions instead of coercing raw
#' values into the closest frame. It further keeps old data for path and polygon
#' type layers so that they are gradually build up instead of being a set of
#' disconnected segments as will happen when using [transition_time()] and
#' [shadow_mark()] together.
#'
#' @param id An unquoted expression giving the id of the row (usually the same
#' as the group aesthetic for lines and polygons)
#' @param along An unquoted expression giving the dimension to tween along. For
#' a gradually revealing time series this should be set to the same as the `x`
#' aesthetic.
#' @param range The time range to animate. If `NULL` it will be set to the range
#' of `along`
#' @param keep_last For non-path/polygon layers should the last row be kept for
#' subsequent frames.
#'
#' @section Label variables:
#' `transition_along` makes the following variables available for string
#' literal interpretation:
#'
#' - **frame_along** gives the position on the along-dimension that the current
#' frame corresponds to
#'
#' @family transitions
#'
#' @examples
#'
#' p <- ggplot(airquality, aes(Day, Temp, group = Month)) +
#'   geom_line() +
#'   transition_reveal(Month, Day)
#'
#' # animate(p)
#'
#' @export
transition_reveal <- function(id, along, range = NULL, keep_last = TRUE) {
  id_quo <- enquo(id)
  along_quo <- enquo(along)
  require_quo(id_quo, 'id')
  require_quo(along_quo, 'along')
  ggproto(NULL, TransitionReveal,
          params = list(
            id_quo = id_quo,
            along_quo = along_quo,
            range = range,
            keep_last = keep_last
          )
  )
}
#' @rdname gganimate-ggproto
#' @format NULL
#' @usage NULL
#' @export
#' @importFrom ggplot2 ggproto
#' @importFrom stringi stri_match
#' @importFrom tweenr tween_along
TransitionReveal <- ggproto('TransitionReveal', Transition,
  mapping = '(.+?)_(.+)',
  var_names = c('id', 'along'),
  setup_params = function(self, data, params) {
    params$id <- get_row_id(data, params$id_quo)
    params$along <- get_row_along(data, params$along_quo, params$nframes, params$range)
    params$require_stat <- is_placeholder(params$id) || is_placeholder(params$along)
    params$row_id <- Map(function(t, i) if (length(t) == 0 || length(i) == 0) character() else paste0(i, '_', format(t)),
                         t = params$along$values, i = params$id$values)
    params
  },
  setup_params2 = function(self, data, params, row_vars) {
    if (is_placeholder(params$id)) {
      params$id <- get_row_id(data, params$id_quo, after = TRUE)
    } else {
      params$id$values <- row_vars$id
    }
    if (is_placeholder(params$along)) {
      params$along <- get_row_along(data, params$along_quo, params$nframes, params$range, after = TRUE)
    } else {
      params$along$values <- row_vars$along
    }
    params$row_id <- Map(function(t, i) if (length(t) == 0 || length(i) == 0) character() else paste0(i, '_', format(t)),
                         t = params$along$values, i = params$id$values)
    params$frame_info <- data.frame(frame_along = params$along$frame_time)
    params
  },
  expand_panel = function(self, data, type, id, match, ease, enter, exit, params, layer_index) {
    split_panel <- stri_match(data$group, regex = '^(.+)<(.+?)_(.+)>(.*)$')
    if (is.na(split_panel[1])) return(data)
    data$group <- paste0(split_panel[, 2], split_panel[, 5])
    time <- as.numeric(split_panel[, 4])
    id <- split_panel[,3]

    all_frames <- switch(
      type,
      point = tween_along(data, ease, params$nframes, !!time, !!id, c(0, 1), FALSE, params$keep_last),
      path = tween_along(data, ease, params$nframes, !!time, !!id, c(0, 1), TRUE, params$keep_last),
      polygon = tween_along(data, ease, params$nframes, !!time, !!id, c(0, 1), TRUE, params$keep_last),
      stop("Unsupported layer type", call. = FALSE)
    )
    all_frames$group <- paste0(all_frames$group, '<', all_frames$.frame, '>')
    all_frames$.frame <- NULL
    all_frames
  }
)


# HELPERS -----------------------------------------------------------------

get_row_along <- function(data, quo, nframes, range, after = FALSE) {
  if (!after && require_stat(quo[[2]])) {
    return(eval_placeholder(data))
  }
  times <- lapply(data, safe_eval, expr = quo)
  times <- standardise_times(times, 'along')
  time_class <- times$class
  times <- times$times
  if (is.null(range)) {
    range <- range(unlist(times))
  } else {
    if (!inherits(range, time_class)) {
      stop('range must be given in the same class as time', call. = FALSE)
    }
    range <- as.numeric(range)
  }
  times <- lapply(times, function(v) {
    if (is.null(v)) return(numeric())
    (v - range[1]) / diff(range)
  })
  frame_time <- seq(range[1], range[2], length.out = nframes)
  frame_time <- recast_times(frame_time, time_class)
  list(values = times, frame_time = frame_time)
}
