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
#' @param along An unquoted expression giving the dimension to tween along. For
#' a gradually revealing time series this should be set to the same as the `x`
#' aesthetic.
#' @param range The time range to animate. If `NULL` it will be set to the range
#' of `along`
#' @param keep_last For non-path/polygon layers should the last row be kept for
#' subsequent frames.
#' @param id **Deprecated**
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
#'   transition_reveal(Day)
#'
#' # animate(p)
#'
#' @export
transition_reveal <- function(along, range = NULL, keep_last = TRUE, id) {
  if (!missing(id)) warning('The `id` argument has been deprecated. Set `id` in each layer with the `group` aesthetic', call. = FALSE)
  along_quo <- enquo(along)
  require_quo(along_quo, 'along')
  ggproto(NULL, TransitionReveal,
          params = list(
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
  mapping = '(.+)',
  var_names = 'along',
  setup_params = function(self, data, params) {
    params$along <- get_row_along(data, params$along_quo, params$nframes, params$range)
    params$require_stat <- is_placeholder(params$along)
    static <- lengths(params$along$values) == 0
    params$row_id <- Map(function(t, s) if (s) character() else t,
                         t = params$along$values, s = static)
    params
  },
  setup_params2 = function(self, data, params, row_vars) {
    if (is_placeholder(params$along)) {
      params$along <- get_row_along(data, params$along_quo, params$nframes, params$range, after = TRUE)
    } else {
      params$along$values <- row_vars$along
    }
    static <- lengths(params$along$values) == 0
    params$row_id <- Map(function(t, s) if (s) character() else t,
                         t = params$along$values, s = static)
    params$frame_info <- data.frame(frame_along = params$along$frame_time)
    params
  },
  expand_panel = function(self, data, type, id, match, ease, enter, exit, params, layer_index) {
    row_vars <- self$get_row_vars(data)
    if (is.null(row_vars)) return(data)
    data$group <- paste0(row_vars$before, row_vars$after)
    time <- as.numeric(row_vars$along)

    all_frames <- switch(
      type,
      point = tween_along(data, ease, params$nframes, !!time, group, c(1, params$nframes), FALSE, params$keep_last),
      path = tween_along(data, ease, params$nframes, !!time, group, c(1, params$nframes), TRUE, params$keep_last),
      polygon = tween_along(data, ease, params$nframes, !!time, group, c(1, params$nframes), TRUE, params$keep_last),
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
  full_length <- diff(range)
  frames <- lapply(times, function(v) {
    if (is.null(v)) return(numeric())
    frame <- round((nframes - 1) * (v - range[1])/full_length) + 1
    nc <- nchar(max(frame))
    sprintf(paste0('%0', nc, 'i'), frame)
  })
  frame_time <- seq(range[1], range[2], length.out = nframes)
  frame_time <- recast_times(frame_time, time_class)
  list(values = frames, frame_time = frame_time)
}
