#' Transition through distinct states in time
#'
#' This is a variant of [transition_states()] that is intended for data where
#' the states are representing specific point in time. The transition length
#' between the states will be set to correspond to the actual time difference
#' between them.
#'
#' @param time An unquoted expression giving the time, and thus state
#' membership, of each observation.
#' @param range The time range to animate. If `NULL` it will be set to the range
#' of `time`
#'
#' @section Label variables:
#' `transition_time` makes the following variables available for string
#' literal interpretation, in addition to the general ones provided by
#' [animate()]:
#'
#' - **frame_time** gives the time that the current frame corresponds to
#'
#' @section Object permanence:
#' `transition_time` uses the group aesthetic of each layer to identify
#' which rows in the input data correspond to the same graphic element and will
#' therefore define which elements will turn into each other between time points.
#' The group aesthetic, if not set, will be calculated from the interaction of all
#' discrete aesthetics in the layer (excluding `label`), so it is often better
#' to set it explicitly when animating, to make sure your data is interpreted in
#' the right way. If the group aesthetic is not set, and no discrete aesthetics
#' exists then all rows will have the same group. If the group aesthetic is not
#' unique in each state, then rows will be matched first by group and then by
#' index. Unmatched rows will appear/disappear, potentially using an enter or
#' exit function.
#'
#' @inheritSection transition_states Computed Variables
#'
#' @family transitions
#'
#' @importFrom ggplot2 ggproto
#' @export
#'
#' @examples
#' anim <- ggplot(airquality, aes(Day, Temp)) +
#'   geom_point(aes(colour = factor(Month))) +
#'   transition_time(Day)
#'
#' # Removing a time point will prolong the tweening between neighbouring time
#' # points so the time dimension stays linear
#' airquality_missing <- airquality[airquality$Day <= 10 | airquality$Day >= 20, ]
#' anim1 <- ggplot(airquality_missing, aes(Day, Temp)) +
#'   geom_point(aes(colour = factor(Month))) +
#'   transition_time(Day)
#'
#' # Range can be constrained if needed
#' anim2 <- ggplot(airquality, aes(Day, Temp)) +
#'   geom_point(aes(colour = factor(Month))) +
#'   transition_time(Day, range = c(10L, 20L))
#'
#' # The group aesthetic is used to connect elements
#' # No grouping
#' anim3 <- ggplot(airquality, aes(Day, Temp)) +
#'   geom_line() +
#'   transition_time(Month)
#'
#' # Group by month
#' anim4 <- ggplot(airquality, aes(Day, Temp)) +
#'   geom_line(aes(group = Month)) +
#'   transition_time(Month) +
#'   enter_fade() +
#'   exit_fade()
transition_time <- function(time, range = NULL) {
  time_quo <- enquo(time)
  require_quo(time_quo, 'time')
  ggproto(NULL, TransitionTime,
          params = list(
            time_quo = time_quo,
            range = range
          )
  )
}
#' @rdname gganimate-ggproto
#' @format NULL
#' @usage NULL
#' @export
#' @importFrom ggplot2 ggproto
#' @importFrom stringi stri_match
#' @importFrom tweenr tween_state keep_state
TransitionTime <- ggproto('TransitionTime', Transition,
  mapping = '(.+)',
  var_names = 'time',
  setup_params = function(self, data, params) {
    params$time <- get_row_time(data, params$time_quo, params$nframes, params$range)
    params$require_stat <- is_placeholder(params$time)
    params$row_id <- params$time$values
    params
  },
  setup_params2 = function(self, data, params, row_vars) {
    if (is_placeholder(params$time)) {
      params$time <- get_row_time(data, params$time_quo, params$nframes, params$range, after = TRUE)
    } else {
      params$time$values <- suppressWarnings(lapply(row_vars$time, as.integer))
    }
    params$row_id <- params$time$values
    params$frame_info <- data_frame0(frame_time = params$time$frame_time)
    params
  },
  expand_panel = function(self, data, type, id, match, ease, enter, exit, params, layer_index) {
    row_time <- self$get_row_vars(data)
    if (is.null(row_time)) return(data)
    data$group <- paste0(row_time$before, row_time$after)
    time <- suppressWarnings(as.integer(row_time$time))
    states <- split(data, time)
    times <- as.integer(names(states))
    nframes <- diff(times)
    nframes[1] <- nframes[1] + 1

    if (times[1] <= 1) {
      all_frames <- states[[1]]
      states <- states[-1]
    } else {
      all_frames <- data[0, , drop = FALSE]
      nframes <- c(times[1] - 1, nframes)
    }
    if (times[length(times)] < params$nframes) {
      states <- c(states, list(data[0, , drop = FALSE]))
      nframes <- c(nframes, params$nframes - times[length(times)])
    }

    for (i in seq_along(states)) {
      all_frames <- switch(
        type,
        point = tween_state(all_frames, states[[i]], ease, nframes[i], !!id, enter, exit),
        path = transform_path(all_frames, states[[i]], ease, nframes[i], !!id, enter, exit, match),
        polygon = transform_polygon(all_frames, states[[i]], ease, nframes[i], !!id, enter, exit, match),
        sf = transform_sf(all_frames, states[[i]], ease, nframes[i], !!id, enter, exit),
        cli::cli_abort('{type} layers not currently supported by {.fun transition_time}')
      )
    }
    true_frame <- seq(max(1, times[1]), min(times[length(times)], params$nframes))
    all_frames <- all_frames[all_frames$.frame %in% true_frame, , drop = FALSE]
    all_frames$group <- paste0(all_frames$group, '<', all_frames$.frame, '>')
    all_frames$.frame <- NULL
    all_frames
  }
)


# HELPERS -----------------------------------------------------------------

get_row_time <- function(data, quo, nframes, range, after = FALSE) {
  if (after || !require_stat(rlang::quo_get_expr(quo))) {
    get_times(data, quo, nframes, range)
  } else {
    eval_placeholder(data)
  }
}
get_times <- function(data, var, nframes, range) {
  times <- lapply(data, safe_eval, expr = var)
  times <- standardise_times(times, 'time')
  time_class <- times$class
  times <- times$times
  if (is.null(range)) {
    range <- range(unlist(times), na.rm = TRUE)
  } else {
    if (!inherits(range, time_class)) {
      cli::cli_abort('{.arg range} must be given in the same class as {.field time}')
    }
    range <- as.numeric(range)
  }
  times <- lapply(times, function(v) {
    if (is.null(v)) return(integer())
    v_u <- unique0(v)
    v_v <- round(1 + (nframes - 1) * (v_u - range[1]) / diff(range))
    v_v[duplicated(v_v)] <- NA
    v_v[match(v, v_u)]
  })
  frame_time <- seq(range[1], range[2], length.out = nframes)
  frame_time <- recast_times(frame_time, time_class)
  list(values = times, frame_time = frame_time)
}
