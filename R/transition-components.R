#' @include transition-manual.R
NULL

#' Transition individual components through their own lifecycle
#'
#' This transition allows individual visual components to define their own
#' "life-cycle". This means that the final animation will not have any common
#' "state" and "transition" phase as any component can be moving or static at
#' any point in time.
#'
#' @param id The unquoted name of the column holding the id that links
#' components across the data
#' @param time The unquoted name of the column holding the time for each state
#' of the components
#' @param range The range the animation should span. Defaults to the range of
#' time plus enter and exit length
#' @param enter_length,exit_length How long time should be spend on enter and
#' exit transitions. Defaults to 0
#'
#' @section Label variables:
#' `transition_components` makes the following variables available for string
#' literal interpretation:
#'
#' - **frame_time** gives the time that the current frame corresponds to
#'
#' @family transitions
#'
#' @export
#' @importFrom rlang enquo
#' @importFrom ggplot2 ggproto
transition_components <- function(id, time, range = NULL, enter_length = NULL, exit_length = NULL) {
  id_quo <- enquo(id)
  time_quo <- enquo(time)
  ggproto(NULL, TransitionComponents,
          params = list(
            id_quo = id_quo,
            time_quo = time_quo,
            range = range,
            enter_length = enter_length,
            exit_length = exit_length
          )
  )
}
#' @rdname gganimate-ggproto
#' @format NULL
#' @usage NULL
#' @export
#' @importFrom ggplot2 ggproto
#' @importFrom stringi stri_match
#' @importFrom tweenr tween_components
TransitionComponents <- ggproto('TransitionComponents', TransitionManual,
  setup_params = function(self, data, params) {
    data_info <- component_info(data, params)

    params$enter_length <- data_info$enter_length
    params$exit_length <- data_info$exit_length
    params$range <- data_info$range
    params$frame_time <- data_info$frame_time
    params$row_id <- Map(function(t, i, s) if (s) character() else paste0(t, '-', i),
                         t = data_info$row_frame, i = data_info$row_id, s = data_info$static)
    params$frame_info <- data.frame(
      frame_time = data_info$frame_time
    )
    params$nframes <- nrow(params$frame_info)
    params
  },
  expand_panel = function(self, data, type, id, match, ease, enter, exit, params, layer_index) {
    split_panel <- stri_match(data$group, regex = '^(.+)<(.+?)-(.+)>(.*)$')
    if (is.na(split_panel[1])) return(data)
    data$group <- paste0(split_panel[, 2], split_panel[, 5])
    time <- as.integer(split_panel[, 3])
    id <- split_panel[, 4]
    all_frames <- switch(
      type,
      point = tween_components(data, ease, params$nframes, !!time, !!id, params$range, enter, exit, params$enter_length, params$exit_length),
      #path = tween_path(all_frames, next_state, es, params$transition_length[i], id, en, ex),
      #polygon = tween_polygon(all_frames, next_state, es, params$transition_length[i], id, en, ex),
      #sf = tween_sf(all_frames, next_state, es, params$transition_length[i], id, en, ex),
      stop("Unsupported layer type", call. = FALSE)
    )
    all_frames$group <- paste0(all_frames$group, '<', all_frames$.frame, '>')
    all_frames$.frame <- NULL
    all_frames
  }
)

component_info <- function(data, params) {
  row_id <- lapply(data, safe_eval, expr = params$id_quo)
  row_time <- lapply(data, safe_eval, expr = params$time_quo)
  static_layer <- unlist(Map(function(data, id, time) {
    (length(id) != 1 && length(id) != nrow(data)) || (length(time) != 1 && length(time) != nrow(data))
  }, data = data, id = row_id, time = row_time))
  if (all(static_layer)) {
    stop('At least one layer must be in transition', call. = FALSE)
  }
  row_id[static_layer] <- list(character(0))
  row_time[static_layer] <- list(numeric(0))
  standard_times <- standardise_times(row_time, 'time')
  range <- if (is.null(params$range)) {
    range(unlist(standard_times$times))
  } else {
    if (!inherits(params$range, standard_times$class)) {
      stop('range must be given in the same class as time', call. = FALSE)
    }
    as.numeric(params$range)
  }
  full_length <- diff(range)
  enter_length <- if (is.null(params$enter_length)) {
    0
  } else {
    if (!inherits(params$enter_length, standard_times$class)) {
      stop('enter_length must be given in the same class as time', call. = FALSE)
    }
    as.numeric(params$enter_length)
  }
  exit_length <- if (is.null(params$exit_length)) {
    0
  } else {
    if (!inherits(params$exit_length, standard_times$class)) {
      stop('exit_length must be given in the same class as time', call. = FALSE)
    }
    as.numeric(params$exit_length)
  }
  row_frame <- lapply(standard_times$times, function(x) {
    round((params$nframes - 1) * (x - range[1])/full_length) + 1
  })
  frame_time <- recast_times(
    seq(range[1], range[2], length.out = params$nframes),
    standard_times$class
  )
  frame_length <- full_length / params$nframes
  list(
    row_id = row_id,
    row_frame = row_frame,
    frame_time = frame_time,
    enter_length = round(enter_length / frame_length),
    exit_length = round(exit_length / frame_length),
    static = static_layer
  )
}

standardise_times <- function(times, name, to_class = NULL) {
  possible_classes <- c('integer', 'numeric', 'POSIXct', 'Date', 'difftime', 'hms')
  classes <- vapply(times[lengths(times) != 0], function(x) {
    cl <- inherits(x, possible_classes, which = TRUE)
    which(cl != 0 & cl == min(cl[cl != 0]))[1]
  }, integer(1))
  if (anyNA(classes)) stop(name, ' data must either be ', paste0(possible_classes[-length(possible_classes)], collapse = ', '), ', or', possible_classes[length(possible_classes)], call. = FALSE)
  if (length(unique(classes)) > 1) stop(name, ' data must be the same class in all layers', call. = FALSE)
  cl <- possible_classes[unique(classes)]
  if (length(cl) == 1 && (cl == 'difftime' || cl == 'hms')) {
    if (is.null(to_class)) {
      lapply(times, `units<-`, 'secs')
    } else if (to_class == 'POSIXct') {
      cl <- to_class
      lapply(times, `units<-`, 'secs')
    } else if (to_class == 'Date') {
      cl <- to_class
      lapply(times, `units<-`, 'days')
    }
  }
  if (!is.null(to_class) && length(cl) != 0) if (cl != to_class) stop(name, ' data must be ', to_class, call. = FALSE)
  list(
    times = lapply(times, as.numeric),
    class = cl
  )
}
recast_times <- function(time, class) {
  switch(
    class,
    integer = as.integer(round(time)),
    numeric =  time,
    POSIXct = structure(time, class = c('POSIXct', 'POSIXt')),
    Date = structure(time, class = 'Date'),
    difftime = structure(time, units = 'secs', class = 'difftime'),
    hms = structure(time, units = 'secs', class = c('hms','difftime'))
  )
}
