# sewing-pattern-editor

[![Build Status](https://travis-ci.org/kirchner/sewing-pattern-editor.svg?branch=master)](https://travis-ci.org/kirchner/sewing-pattern-editor)

Create sewing patterns in your browser. [Live
Demo](https://sewing-pattern-editor.herokuapp.com)

Note, that this is in a very early development state.  Nevertheless,
I appreciate any questions or feedback. :)

This project got a lot of inspiration and knowledge from
[Seamly2D](https://github.com/FashionFreedom/Seamly2D) and
[TauMeta](https://github.com/slspencer/TauMeta), so you should definitely check
these out, too!


## What is this all about?

When constructing sewing patterns, one usually takes some measurments of the
person the final clothes should be for (e.g. shoulder width, back width, leg
length) and then follows some construction steps, like 'draw a point', 'draw
a line from point A downwards of half the length of the back height', 'draw
a line perpendicular to some other line at 2/3 of that line', etc..

Eventually, you should be able to feed this program all these construction
steps, so that you only have to update the input measurements to adjust your
sewing patterns.


## Run locally

You can run the development server with

```
$ yarn
$ yarn run dev
```

Then just visit `localhost:1234` in your browser.
