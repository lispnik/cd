# cd — Common Lisp bindings to Canvas Draw

CFFI bindings to [CD](https://www.tecgraf.puc-rio.br/cd/), Tecgraf's Canvas
Draw: a 2D vector graphics library that draws the same picture to a file, an
image buffer or a window, through interchangeable drivers.

This project is unaffiliated with Tecgraf.

Built against [lispnik/tecgraf-cd](https://github.com/lispnik/tecgraf-cd), a
CMake fork of CD 5.14.

## Requirements

- SBCL, and [ocicl](https://github.com/ocicl/ocicl) for dependencies
- CD 5.14 shared library, and the IM library it is built against
- The [im](https://github.com/lispnik/im) bindings, which this depends on

```sh
ocicl install
CD_LIBRARY_PATH=/path/to/tecgraf-cd/build sbcl --eval '(asdf:load-system :cd)'
```

## Finding the library

Searched in order: `cd:*library-path*`, the `CD_LIBRARY_PATH` environment
variable, a `lib/` directory or the directory beside the running executable,
then CFFI's own search path.

## Drivers

CD compiles its drivers **into** libcd according to CMake options rather than
shipping them separately, so which ones exist is decided when the C library is
built and can differ between two copies of the same version.

```lisp
(cd:drivers)
;; => ("SVG" "PS" "PDF" "METAFILE" "PICTURE" "IMAGE" "IMAGERGB" ...)

(cd:driver-available-p "PDF")   ; => T
```

Asking a missing driver for a canvas signals `cd:driver-not-available` rather
than handing back a NULL context.

## Canvases

```lisp
(cd:with-canvas (c (cd:svg-canvas #p"plot.svg" :width-mm 200 :height-mm 150))
  (setf (cd:foreground c) :dark-blue
        (cd:line-width c) 2)
  (cd:line c 0 0 100 100)
  (cd:box c 20 80 20 60)
  (cd:text c 50 120 "hello"))
```

A canvas is a CLOS object released by `cd:kill`, by `cd:with-canvas` on
unwind, or as a last resort by a finalizer. For the file drivers that release
is not merely tidiness: CD writes the file's trailer in `cdKillCanvas`, so a
canvas that is never killed leaves a truncated file no reader will accept.

### Driver constructors

CD creates a canvas from a context and a driver-specific *data string*, each
driver inventing its own syntax and reporting a malformed one by returning
NULL with no explanation:

```c
cdCreateCanvas(cdContextPS(),  "out.ps -pA4 -s300 -e");
cdCreateCanvas(cdContextSVG(), "out.svg 200x150 3.5");
```

So this binding does not ask you to write them:

```lisp
(cd:svg-canvas        #p"out.svg" :width-mm 200 :height-mm 150 :resolution 3.5)
(cd:postscript-canvas #p"fig.eps" :paper :a4 :encapsulated t :resolution 300)
(cd:pdf-canvas        #p"out.pdf" :paper :letter :landscape t)
(cd:metafile-canvas   #p"rec.cdm" :width-mm 100 :height-mm 100)
(cd:image-rgb-canvas  640 480)
(cd:picture-canvas)
(cd:cgm-canvas #p"o.cgm" :binary t)   (cd:dxf-canvas #p"o.dxf")
(cd:dgn-canvas #p"o.dgn")             (cd:debug-canvas #p"trace.log")
```

`cd:make-canvas` takes a driver name and a raw data string, for drivers not
wrapped above or when you would rather write the string yourself.

## Drawing

Every operation takes the canvas it acts on. CD's global "active canvas" API is
not exposed: it cannot be made safe with more than one canvas or more than one
thread, and each of its functions has a `cdCanvas*` counterpart wrapped here.

```lisp
(cd:line c 0 0 100 100)          (cd:box c 10 90 10 90)
(cd:rect c 10 90 10 90)          (cd:arc c 50 50 40 30 0 180)
(cd:sector c 50 50 40 40 45 270) (cd:chord c 50 50 40 40 0 90)
(cd:mark c 25 25)                (cd:pixel c 5 5 :yellow)
(cd:text c 50 80 "label")

(cd:with-shape (c :fill)
  (cd:vertex c 10 10) (cd:vertex c 90 10) (cd:vertex c 50 80))
```

Primitives dispatch on their arguments: integers use CD's integer entry point,
anything else the `cdf*` double one. The distinction is real — the integer path
is what a pixel driver wants, the double path what a vector driver can honour
exactly — so it is decided by what you pass rather than by which name you
remember.

`with-shape` ends the shape on unwind. That matters more than the usual
argument for such a macro: a canvas left mid-shape has CD accumulating
vertices, and the next unrelated drawing call joins the polygon.

### Colours

Anywhere a colour is wanted, three spellings are interchangeable:

```lisp
(setf (cd:foreground c) :red)          ; a name
(setf (cd:foreground c) '(255 0 0))    ; components, 0-255
(setf (cd:foreground c) '(255 0 0 128)); with alpha
(setf (cd:foreground c) (cd:encode-color 255 0 0))  ; already packed
```

### Attributes

Readers with `setf` pairs, hiding CD's `CD_QUERY` sentinel, taking and
returning keywords:

```lisp
(setf (cd:line-width c) 3
      (cd:line-style c) :dashed        ; :continuous :dotted :dash-dot ...
      (cd:line-join c) :bevel          ; :miter :round
      (cd:interior-style c) :hatch     ; :solid :stipple :pattern :hollow
      (cd:hatch-style c) :cross
      (cd:text-alignment c) :center)

(cd:font c :face "Helvetica" :style :bold :size 12)
(cd:text-size c "hello")               ; => width, height
```

### World coordinates

Draw in the units the problem is in, and let CD map them onto the canvas:

```lisp
(cd:with-wd-window (c -1.0 1.0 -1.0 1.0)
  (cd:wd-line c -0.5 -0.5 0.5 0.5)
  (cd:wd-text c 0.0 0.8 "centred"))

(cd:wd-world-to-canvas c 0.0 0.0)      ; => pixel coordinates
```

These keep the `wd-` prefix rather than overloading the pixel names, because
they are different functions against the same canvas and a caller has to know
which space they are in.

### Clipping and transforms

```lisp
(cd:with-clip-area (c 10 100 10 100)
  (cd:line c 0 0 200 200))             ; clipped to the rectangle

(cd:with-transform (c)
  (cd:transform-rotate c 45)
  (cd:transform-scale c 2 2)
  (cd:line c 0 0 50 0))
```

Both restore what they changed. CD's transform and clipping mode are canvas
state, so a function that sets one and does not put it back changes the meaning
of every later call.

Setting a clipping rectangle does not by itself switch clipping on — CD keeps
the area and the mode separate, which is why `with-clip-area` does both.

## Images

With the IM bindings, which this depends on:

```lisp
(im:with-image (photo (im:load #p"photo.jpg"))
  (cd:put-image c photo :x 0 :y 0))

(im:with-image (grabbed (cd:capture-image c :width 64 :height 64))
  (im:save grabbed #p"grabbed.png"))

(cd:pattern-image c tile)              ; fill with an image
(cd:stipple-image c mask)
```

Or straight from Lisp arrays, three planes rather than interleaved pixels
because that is how both CD and IM store an image:

```lisp
(cd:put-image-rgb c width height red green blue)
(cd:get-image-rgb c width height)      ; => three (unsigned-byte 8) vectors
```

Only drivers holding a raster can be read back. PostScript and SVG have no
pixels, and CD answers by leaving the buffers untouched rather than failing —
so all zeros from a vector driver is CD declining, not a black image.

## Recording and replay

A metafile or picture canvas records drawing instead of rasterising it, and
`play` replays it through any other driver. That is CD's answer to converting
between vector formats:

```lisp
(cd:with-canvas (c (cd:metafile-canvas #p"drawing.cdm" :width-mm 100 :height-mm 100))
  (cd:line c 0 0 200 200))

(cd:with-canvas (out (cd:svg-canvas #p"drawing.svg" :width-mm 100 :height-mm 100))
  (cd:play-file out #p"drawing.cdm"))
```

Drivers can expose callbacks during replay. The identifiers are numbered **per
driver** — `1` is `CD_CGMCOUNTERCB` to the CGM driver and nothing at all to the
metafile one — so `register-callback` signals `cd:unsupported-operation` rather
than installing something that would never fire:

```lisp
(cd:with-callback ("CGM" :cgm-counter (lambda (canvas) t))
  (cd:play-file target #p"drawing.cgm" :driver "CGM"))
```

## Conditions

Every failure is a subtype of `cd:cd-error`. CD reports failure thinly -- there
is no error-code enum, `cdCreateCanvas` returns NULL and most drawing calls
return nothing at all -- so the classes describe what the binding could
determine rather than a code the library handed over:

| Condition | Means |
|---|---|
| `cd:driver-not-available` | this build of CD has no such driver |
| `cd:canvas-creation-error` | `cdCreateCanvas` returned NULL |
| `cd:invalid-canvas` | drawing on a canvas already killed |
| `cd:unsupported-operation` | the driver does not implement it |
| `cd:library-not-found` | libcd could not be opened; lists what was tried |

## Layout

| Path | Contents |
|---|---|
| `src/ffi/` | The raw bindings. Generated, then hand-corrected. **Do not add hand-written files here** — the generator clears it. |
| `src/*.lisp` | The Lisp API: conditions, library loading, canvases and drivers. |
| `tools/gen-bindings.lisp` | The binding generator. Not part of any shipped system. |

### Regenerating

```sh
sbcl --non-interactive --load tools/gen-bindings.lisp \
     --eval '(cd.gen:generate "/path/to/tecgraf-cd")'
```

The generator takes its symbol list from `nm` on the **built library**, not
from the headers, so it cannot bind a function that does not exist — the
previous binding had ten that did not, `cdCanvasBezier` and `cdCanvasSpline`
among them.

It also decides what *ought* to be bound by reading which header declares each
symbol. libcd exports around 475 `cd*`/`wd*` names but only ~243 are public
API, because it is one shared object with no symbol visibility: the rest are
driver internals from `cd_private.h` (`cdSimArc`, `cdStrDup`, `cdTT_load`) and
the pre-5.0 global API from `cd_old.h`. Without that split a coverage report is
noise. With it, the report reads *public but unbound: 0*.

**Which build you generate against matters.** Drivers are compiled in per
CMake option, so a tree configured with `CD_ENABLE_IM=OFF` exports no
`cdCanvasPutImImage` and the generator would correctly emit no IM bridge.
Generate against the most fully-featured build you have.

## Deviations from the C API

- The global "active canvas" API (`cdLine`, `cdBox`, `cdActivate`) is
  deliberately not exposed. It cannot be made safe with more than one canvas or
  more than one thread, and every one of its functions has a `cdCanvas*`
  counterpart taking the canvas explicitly.
- Enum members are keywords named after the group CD documents them under:
  `:polygon-mode-fill`, `:line-join-bevel`, `:text-alignment-center`.
- `cdCanvas*` and `wdCanvas*` keep their prefixes (`cd:line` vs `cd:wd-line`),
  because they are different functions — pixels versus world coordinates — and
  collapsing them would silently bind one over the other.

## License

MIT. See LICENSE.
