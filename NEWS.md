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
