# gganimate 1.0.9

* Fix for using `"svg"` device during knitting (#484)
* Fix for correctly getting chunk options during knitting (#485)
* Fix a bug in `transition_reveal()` that would remove data during transitions
  (#480 and #473)
* General upkeep to keep it in line with the evolvling coding principles in 
  ggplot2 (move to using vctrs, cli, lifecycle, etc)
* `transition_reveal()` now throws an error when it is used in conjunction with
  `stat_align()` and transitioning before the stat has been calculated (#476)
* Label interpolation now works when labels are expressions (#439)
* `as_html_video()` gains `muted`, `loop`, and `controls` argument to control 
  presentation of the video. Like autoplay these can be controlled during knitr
  through the chunk options (e.g. `gganimate = list(muted = TRUE, loop = TRUE)`)
  (#444)
* Make the build process up-to-date with ggplot2 3.5.0

# gganimate 1.0.8

* Fix a bug when creating labels from aesthetics that include glue expressions 
  (#422)
* Fix a bug in ffmpeg detection (#346, @rfaelens, #360, @adamdsmith)
* Remove plyr dependency
* Fix a bug in `transition_filter()` in the presence of `NA` filter values 
  (#404, @rsaporta)
* Fix a bug with static layers that include position adjustments (#418)
* Fix a bug in `transition_time()` where multiple time values in the same frame
  would lead to unexpected stacking of the data (#414)
* Fix a bug in `transition_reveal()` where coinciding tweens would result in 
  wrong direction of the arrow (#409)
* Fix a bug in `transition_layer()` when the last layer had an exit duration 
  (#384)
* Fix a bug in `transition_time()` when facets had data with different starting
  time (#357)
* Make sure `exclude_layer` in `view_*()` is taken into account
* Fix a bug when using a view with `coord_flip()` which would result in weird 
  flickering and movement of the position guies (#336)
* Fix a bug in `transition_reveal()` that would require input to be ordered 
  along the time dimension for point-like geoms (#323)
* Fix a bug in `view_follow()` that would result in errors when used with 
  discrete scales (#304)
* Fix bug in `transition_states()` where too few frames relative to states in 
  combination with `wrap = FALSE` would cause an error (#301)
* Fix a bug in `transition_time()` when the time dimension contained `NA` values
  (#307)

# gganimate 1.0.7

* Support ragg png device to ensure that it works with knitr's ragg support

# gganimate 1.0.6

# gganimate 1.0.5

* No longer needs the png package for gifski rendering
* Give startup message if no renderer backend are detected.

# gganimate 1.0.4

* Fix a bug in `ffmpeg_renderer()` where the `fps` argument were being ignored.
* Make sure `ggraph` objects registers the graph context after they are promoted
  to `gganim` objects.
* Better fallback strategy for lack of gifski.

# gganimate 1.0.3

* Fix a conditional in the plot building step to make it work with the ggplot2
  3.1.1 hotfix.

# gganimate 1.0.2

* Fix registration of S3 methods for suggested packages.

# gganimate 1.0.1

* Fix bug in transitions when the group aesthetic was late evalued (#262)
* Better fade support for `geom_smooth()`. Both ribbon and line will now fade.
* Fix bug in transitions when group aesthetic was a string that could be 
  interpreted as a double (#266)
* Fix bug in `shadow_mark()` where future shadows were drawn above the main 
  frame data.
* Better error messages when a layer type is not supported by the transition
* Fix bug where transitions didn't work with difftime/hms for specifying 
  durations when transition variable was a POSIX or date class.
* Fix bug where using a view would modify the plot coordinate system in-place,
  resulting in modifications to the original plot object.
* `view_follow` now works with transformed scales.

# gganimate 1.0.0

* First CRAN release, featuring a complete rewrite... Too much stuff to put in
  a changelog
