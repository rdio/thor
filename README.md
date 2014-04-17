Thor
====

A Finagle-based image processor.

NOTE: JDK7 or higher required. Run `java -version` and check that it shows at least `1.7`.

Installation
---

1. Clone the repo: `git clone https://github.com/rdio/thor.git`
2. Add your `application.conf` to `src/main/resources` and configure a media host (see `reference.conf` for default properties or https://github.com/typesafehub/config/ for more documentation)
3. Run the app: `sbt run`
4. Navigate to a valid relative path on your media provider `http://localhost:8080/?l=my_img.jpg` to verify that its working

Querystring Params
---

- `w` - width of the final image
- `h` - height of the final image
- `c` - compression quality to apply to jpeg image results (0-100)
  - NOTE: File format is chosen automatically based on whether or not a filter or layer utilizes transparency.
- `l` - `<layer>[; <layer>]+` - a sequence of one or more layers separated by a semi-colon: `;`
  - layer - `<source>[:<filter>]|<filter>` - a layer consists of a single image source and an optional filter separated by a colon: `:`, or a single filter without a source
    - source - `<url>|$<index>|_` - either a relative url path or a layer reference or an empty layer
      - url - any relative url accessible by the media provider
      - layer reference - `$<index>` - zero-based index into any _preceeding_ layer
      - empty layer — `_` - a blank layer the size of the final output with pixels initialized to fully transparent
    - filter - `<filter-name>([args])` - filter to apply to the current layer or if no source was provided, the previous layer
      - linear gradient - `linear(<angle>, <color-stop>, <color-stop> [, <color-stop>]+)` - applies a linear gradient over the layer at `<angle>` degrees with _at least_ two color stops
        - color stop - `<rgb|rgba> <percentage>%` - supports either rgb or rgba colors
      - blur - `blur()` - blurs the image using a 3x3 convolution kernel
      - boxblur - `blur(<horizontal-radius-pixels>, <vertical-radius-pixels>)` - applies a box blur to the image by `radius-pixels`
      - scale - `scale(<percent>%)` - resizes the image by `percent`
      - scaleto - `scaleto(<width-pixels>, <height-pixels>)` - resizes the image to the specified dimensions
      - grid - `grid(<source>[, <source>]+)` - forms a grid from the list of source images (original layer included in the grid)
      - round - `round(<radius-pixels>px)` - rounds the corners of the layer by `radius-pixels`
      - pad - `pad(<padding-pixels>)` - applies padding around the image by `padding-pixels`
      - colorize - `colorize(<color>)` - overlays the specified `color` over the image
      - overlay - `overlay(<source>)` - overlays the specified `source` over the image
      - mask - `mask(<overlay-source>, <mask-source>)` - blends between the layer and `overlay-source` using `mask-source` as a guide

Examples
---

(all querystring params have been url encoded, e.g. `encodeURIComponent` in javascript)

- http://localhost:8080/?l=img_a.jpg%3Bboxblur(40px%2C40px)
  - Blurs img_a.jpg using a 40x40 box blur
- http://localhost:8080/?l=img_a.jpg%3Bimg_b.jpg%3B_%3Alinear(0deg%2C%20rgba(0%2C%200%2C%200%2C%200)%200%25%2C%20rgba(0%2C%200%2C%200%2C%201)%20100%25)%3B%240%3Amask(%241%2C%20%242)
  - Linearly interpolates between two images (img_a.jpg and img_b.jpg) using a linear gradient as a blending mask. Layers:
    1. `img_a.jpg`
    2. `img_b.jpg`
    3. `_:linear(0deg, rgba(0, 0, 0, 0) 0%, rgba(0, 0, 0, 1) 100%) # on an empty layer, create a linear gradient that fades from fully transparent black to fully opaque black`
    4. `$0:mask($1, $2) # Blend between layers $0 and $1 using layer $2 as a mask`

License
---

    Copyright 2014 Rdio, Inc.

    Licensed under the Apache License, Version 2.0 (the "License");
    you may not use this file except in compliance with the License.
    You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software
    distributed under the License is distributed on an "AS IS" BASIS,
    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and
    limitations under the License.