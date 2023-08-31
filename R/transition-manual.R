#' Create an animation by specifying the frame membership directly
#'
#' This transition allows you to map a variable in your data to a specific frame
#' in the animation. No tweening of data will be made and the number of frames
#' in the animation will be decided by the number of levels in the frame
#' variable.
#'
#' @param frames The unquoted name of the column holding the frame membership.
#' @param ... Additional variables
#' @param cumulative Keep data from previous frames as part of the current frame
#' data
#'
#' @family transitions
#'
#' @section Label variables:
#' `transition_states` makes the following variables available for string
#' literal interpretation, in addition to the general ones provided by
#' [animate()]:
#'
#' - **previous_frame** The name of the last frame the animation was at
#' - **current_frame** The name of the current frame
#' - **next_frame** The name of the next frame in the animation
#'
#' @section Object permanence:
#' `transition_manual` does not link rows across data to the same graphic
#' element. Every frame is a discrete state and no animation between the states
#' is done.
#'
#' @inheritSection transition_states Computed Variables
#'
#' @importFrom rlang enquo
#' @importFrom ggplot2 ggproto
#' @export
#'
#' @examples
#' anim <- ggplot(mtcars, aes(factor(gear), mpg)) +
#'   geom_boxplot() +
#'   transition_manual(gear)
#'
#' # Using `cumulative = TRUE` to keep data from older frames
#' anim2 <- ggplot(mtcars, aes(factor(gear), mpg)) +
#'   geom_boxplot() +
#'   transition_manual(gear, cumulative = TRUE)
#'
#' # Use `factor()` to set the order of the frames
#' anim3 <- ggplot(mtcars, aes(factor(gear), mpg)) +
#'   geom_boxplot() +
#'   transition_manual(factor(gear, levels = c('4', '3', '5')))
#'
transition_manual <- function(frames, ..., cumulative = FALSE) {
  frames_quo <- enquo(frames)
  require_quo(frames_quo, 'frames')
  frame_vars <- data_frame0(...)
  ggproto(NULL, TransitionManual, params = list(frames_quo = frames_quo, frame_vars = frame_vars, cumulative = cumulative))
}
#' @rdname gganimate-ggproto
#' @format NULL
#' @usage NULL
#' @export
#' @importFrom ggplot2 ggproto
#' @importFrom stringi stri_match
TransitionManual <- ggproto('TransitionManual', Transition,
  mapping = '(.*)',
  var_names = 'frames',
  setup_params = function(self, data, params) {
    params$frames <- get_row_frames(data, params$frames_quo)
    params$reuire_stat <- is_placeholder(params$frames)
    params$row_id <- params$frames$values
    params
  },
  setup_params2 = function(self, data, params, row_vars) {
    if (is_placeholder(params$frames)) {
      params$frames <- get_row_frames(data, params$frames_quo, after = TRUE)
    } else {
      params$frames$values <- lapply(row_vars$frames, as.integer)
    }
    all_frames <- params$frames$levels
    params$row_id <- params$frames$values
    params$frame_info <- data_frame0(
      previous_frame = c('', all_frames[-length(all_frames)]),
      current_frame = all_frames,
      next_frame = c(all_frames[-1], '')
    )
    if (nrow(params$frame_vars) != 0) {
      if (nrow(params$frame_info) != nrow(params$frame_vars)) {
        cli::cli_abort('Additional frame variables must have the same length as the number of frames')
      }
      params$frame_info <- cbind(params$frame_info, params$frame_vars)
    }
    params$nframes <- nrow(params$frame_info)
    params
  },
  expand_panel = function(self, data, type, id, match, ease, enter, exit, params, layer_index) {
    if (!params$cumulative) return(data)
    row_state <- self$get_row_vars(data)
    if (is.null(row_state)) return(data)
    data$group <- paste0(row_state$before, row_state$after)
    state <- as.integer(row_state$frames)
    states <- split(seq_len(nrow(data)), state)
    all_frames <- vec_rbind0(!!!lapply(seq_along(states), function(i) {
      index <- unlist(states[seq_len(i)])
      frame <- data[index, , drop = FALSE]
      frame$.frame <- i
      frame
    }))
    all_frames$group <- paste0(all_frames$group, '<', all_frames$.frame, '>')
    all_frames$.frame <- NULL
    all_frames
  }
)

get_row_frames <- function(data, quo, after = FALSE) {
  if (after || !require_stat(rlang::quo_get_expr(quo))) {
    combine_levels(data, quo)
  } else {
    eval_placeholder(data)
  }
}
