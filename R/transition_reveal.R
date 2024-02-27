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
#'
#' @section Label variables:
#' `transition_reveal` makes the following variables available for string
#' literal interpretation, in addition to the general ones provided by
#' [animate()]:
#'
#' - **frame_along** gives the position on the along-dimension that the current
#' frame corresponds to
#'
#' @section Object permanence:
#' `transition_reveal` uses the group aesthetic of each layer to identify
#' which rows in the input data correspond to the same graphic element and will
#' therefore define a whole to be revealed over the animation.
#' The group aesthetic, if not set, will be calculated from the interaction of all
#' discrete aesthetics in the layer (excluding `label`), so it is often better
#' to set it explicitly when animating, to make sure your data is interpreted in
#' the right way. If the group aesthetic is not set, and no discrete aesthetics
#' exists then all rows will have the same group.
#'
#' @inheritSection transition_states Computed Variables
#'
#' @family transitions
#'
#' @export
#' @importFrom ggplot2 ggproto
#'
#' @examples
#' anim <- ggplot(airquality, aes(Day, Temp, group = Month)) +
#'   geom_line() +
#'   transition_reveal(Day)
#'
#' # Non-paths will only show the current position, not the history
#' anim1 <- ggplot(airquality, aes(Day, Temp, group = Month)) +
#'   geom_line() +
#'   geom_point(colour = 'red', size = 3) +
#'   transition_reveal(Day)
#'
#' # Points can be kept by giving them a unique group and set `keep = TRUE` (the
#' # default)
#' anim2 <- ggplot(airquality, aes(Day, Temp, group = Month)) +
#'   geom_line() +
#'   geom_point(aes(group = seq_along(Day))) +
#'   geom_point(colour = 'red', size = 3) +
#'   transition_reveal(Day)
#'
#' # Since ggplot2 3.4 geom_ribbon and geom_area has used stat_align
#' # This stat is incompatible with transition_reveal when applied before
#' # stats are calculated
#' anim3 <- ggplot(airquality, aes(Day, Temp, group = Month)) +
#'   geom_area() +
#'   transition_reveal(Day)
#'
#' # This can be fixed by either reverting to use stat_identity
#' anim4 <- ggplot(airquality, aes(Day, Temp, group = Month)) +
#'   geom_area(stat = "identity") +
#'   transition_reveal(Day)
#'
#' # Or by applying the transition after the stat
#' anim5 <- ggplot(airquality, aes(Day, Temp, group = Month)) +
#'   geom_area() +
#'   transition_reveal(after_stat(x))
#'
transition_reveal <- function(along, range = NULL, keep_last = TRUE) {
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
      if (any(params$stat_align_layer)) {
        cli::cli_abort(
          c(
            "{.fun transition_reveal} cannot do pre-stat transitioning when using {.fun stat_align}",
            i = "Set {.arg along} to a computed aesthetic, e.g. {.code along = after_stat(x)}, or",
            " " = "use {.fun stat_identity} in layer{?s} {as.character(which(params$stat_align_layer))}"
          ),
          call = call2("transition_reveal")
        )
      }

      params$along$values <- row_vars$along
    }
    static <- lengths(params$along$values) == 0
    params$row_id <- Map(function(t, s) if (s) character() else t,
                         t = params$along$values, s = static)
    params$frame_info <- data_frame0(frame_along = params$along$frame_time)
    params
  },
  expand_panel = function(self, data, type, id, match, ease, enter, exit, params, layer_index) {
    row_vars <- self$get_row_vars(data)
    if (is.null(row_vars)) return(data)
    data$group <- paste0(row_vars$before, row_vars$after)
    time <- as.numeric(row_vars$along)
    if (type == 'point') {
      rank <- order(data$group, time)
      data <- data[rank, ]
      time <- time[rank]
    }

    all_frames <- switch(
      type,
      point = tween_along(data, ease, params$nframes, !!time, group, c(1, params$nframes), FALSE, params$keep_last),
      path = tween_along(data, ease, params$nframes, !!time, group, c(1, params$nframes), TRUE, params$keep_last),
      polygon = tween_along(data, ease, params$nframes, !!time, group, c(1, params$nframes), TRUE, params$keep_last),
      cli::cli_abort('{type} layers not currently supported by {.fun transition_reveal}')
    )
    all_frames$group <- paste0(all_frames$group, '<', all_frames$.frame, '>')
    all_frames$.frame <- NULL
    transitions <- setdiff(which(all_frames$.phase == "transition"), 1L)
    transitions <- transitions[all_frames$.phase[transitions-1L] == "raw" & all_frames$group[transitions-1L] == all_frames$group[transitions]]
    possible_repeats <- sort(c(transitions, transitions - 1L))
    repeated <- duplicated(all_frames[possible_repeats, names(all_frames) != '.phase'], fromLast = TRUE)
    repeated <- possible_repeats[repeated]
    if (length(repeated) == 0) all_frames else all_frames[-repeated, ]
  }
)


# HELPERS -----------------------------------------------------------------

get_row_along <- function(data, quo, nframes, range, after = FALSE) {
  if (!after && require_stat(rlang::quo_get_expr(quo))) {
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
      cli::cli_abort('{.arg range} must be given in the same class as {.field time}')
    }
    range <- as.numeric(range)
  }
  full_length <- diff(range)
  frames <- lapply(times, function(v) {
    if (length(v) == 0) return(numeric())
    frame <- round((nframes - 1) * (v - range[1])/full_length) + 1
    nc <- nchar(max(frame)) + 1
    sprintf(paste0('%0', nc, 'i'), frame)
  })
  frame_time <- seq(range[1], range[2], length.out = nframes)
  frame_time <- recast_times(frame_time, time_class)
  list(values = frames, frame_time = frame_time)
}
