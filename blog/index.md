---
title: Home
---

### Overview

This is a project where I build a "falling blocks" game.

It's an excuse for me to play around with the `sdl2`, `frpnow` and `gl` packages - and a few others along the way - while showing a bit of my working in case anyone out there is interested.

### Posts

#### Refactoring the `sdl2` example

To get started, we'll take the initial example from the `sdl2` package and refactor it.

The goal is to get a good separation of concerns, through which we'll also get to see a number of the packages that will be used throughout this series.

- [Breaking up the initial example](./posts/part1/refactor.html)
- [Expanding on the example](./posts/part1/cycle.html)
- [Adding `mtl`](./posts/part1/mtl.html)
- [Adding `lens`](./posts/part1/lens.html)
<!--
- [Driving the updates with a timer](./posts/part1/timer.html)
-->

#### Coming soon

- More to follow...

### Links

#### SDL2

- The [`sdl2` package](https://hackage.haskell.org/package/sdl2)

#### FRP

- The [`frpnow` package](http://hackage.haskell.org/package/frpnow)
- The original [FRPNow paper (PDF)](http://www.cse.chalmers.se/~atze/papers/prprfrp.pdf) is great
- [This post](https://alfredodinapoli.wordpress.com/2011/12/24/functional-reactive-programming-kick-starter-guide/) really got me motivated to give FRP a proper go

#### GL

- The [`gl` package](https://hackage.haskell.org/package/gl)
- [This write up](http://dpwright.com/posts/2015/03/25/the-haskell-gl-package/) is what I'm planning on using when I get up to using some GL
- [This post](https://ocharles.org.uk/blog/posts/2013-12-02-24-days-of-hackage-linear.html) also looks like it might be useful

