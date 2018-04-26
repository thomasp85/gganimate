
<!-- README.md is generated from README.Rmd. Please edit that file -->
gganimate
=========

[![Travis-CI Build Status](https://travis-ci.org/thomasp85/gganimate.svg?branch=master)](https://travis-ci.org/thomasp85/gganimate) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/thomasp85/gganimate?branch=master&svg=true)](https://ci.appveyor.com/project/thomasp85/gganimate) [![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version-ago/gganimate)](https://cran.r-project.org/package=gganimate) [![CRAN\_Download\_Badge](http://cranlogs.r-pkg.org/badges/gganimate)](https://cran.r-project.org/package=gganimate) [![Coverage Status](https://img.shields.io/codecov/c/github/thomasp85/gganimate/master.svg)](https://codecov.io/github/thomasp85/gganimate?branch=master)

This is the home of the development version of gganimate. It is destined to deviate completely from the prior version and will be a complete rewrite. It goes without saying that this is a **Work in Progress**.

When I get the time I will outline my thoughts and ideas in this readme so it can serve as a whitepaper for what I have in mind - I haven't got the time now...

Old API
-------

I'll start by briefly describe the old API that this version plans to deprecate. The center of this API was the ability to quickly make animations out of `ggplot2` plots by adding a `frame` aesthetic to the layers that should be animated. When the plot was processed by the `gganimate` function the different levels in `frame` would then make up the different frames in the animation. Layers without a `frame` aesthetic would be kept constant. The actual animation was created with the `gganimate` function which took rendering specific arguments such as the framerate, file format etc.

While this approach seems natural to `ggplot2` --- defining aesthetic mappings are second hand to users of `ggplot2` after all --- it has some drawbacks. Chief of these is lack of control with how the animation should proceed. There is no equivalent to the `scale_*_*()` functions to determine how the different aesthetics should transition to new values and the user was left with preprocessing their data with `tweenr` if smooth animations were needed. Another smaller annoyance was that `frame` is an unrecognized aesthetic and `ggplot2` would throw warnings.

The biggest problem in my eyes though, is that the approach felt wrong on a theoretical level. In order to fit into `ggplot2` `gganimate` adopted the most obvious grammar element - the aesthetic - but animations can not be seen solemnly as akin to colour or size. It needs a grammar of its own...

*The above may seem harsh on the original implementation. It is not. gganimate has been very successful and I feel I need to justify why I want to break it completely. Thus a survey of its shortcomings are needed.*

A Grammar of Animated Graphics
------------------------------

So what do I mean when animations needs a grammar of its own? I mean that we need to break down the act of animating a data display into distinct parts and define verbs that denote the different operations. In the end we need functions that fits naturally into the `ggplot2` API that can augment our visualisations with animations while giving us the control we have come to expect from working with `ggplot2`.

It may seem that this is all related to `ggvis`/`vega` and the quest for a grammar of *interactive* graphics --- it is not. The grammar of animated graphics will concern itself solemnly with describing transitions of data displays between a priori known states. On the other hand, a grammar of interactive graphics is concerned with how to describe human interactions with data displays. Often animations are a part of interactive graphics but we should not confuse the two distinct operations.
