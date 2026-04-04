# multifile — multi-file module pattern examples

Three progressively larger examples that demonstrate how Hew's module system
scales from a single file to a layered directory structure.

## Examples at a glance

| # | Directory | What it teaches |
|---|-----------|-----------------|
| 1 | `01_shapes/` | Directory-form module; peer files contribute types + trait impls |
| 2 | `02_geometry/` | Peer-file type sharing; selective `import mod::{A, B}` |
| 3 | `03_text_stats/` | Two-level hierarchy; `import parent;` vs `import parent::child;` |

---

## 01_shapes — directory module with peer-file types

```
01_shapes/
  shapes/
    shapes.hew       Drawable trait + render() helper
    circle.hew       Circle type + impl Drawable + make_circle()
    rectangle.hew    Rectangle type + impl Drawable + make_rectangle()
  main.hew
```

**Pattern:** a directory named `shapes/` becomes the `shapes` module.
`shapes.hew` is the entry file (its stem matches the directory name).
`circle.hew` and `rectangle.hew` are **peer files** — they merge into the
same `shapes` namespace automatically; no extra `import` is needed inside
the module itself.

`main.hew` uses a bare import and qualified access:

```hew
import shapes;

let c = shapes.make_circle(5);
println(shapes.render(c));   // "Circle(r=5)"
```

Run it:
```sh
hew run   examples/multifile/01_shapes/main.hew
hew check examples/multifile/01_shapes/main.hew
```

Expected output:
```
Circle(r=5)
Rectangle(4x3)
Circle(r=100)
```

---

## 02_geometry — selective imports + peer-file type sharing

```
02_geometry/
  geo/
    geo.hew          Point type + origin()
    distance.hew     abs_int, manhattan, chebyshev  (uses Point — no import)
    area.hew         bounding_area                   (uses Point + abs_int — no import)
  main.hew
```

**Key insight:** peer files inside a directory module share their namespace.
`distance.hew` uses `Point` (defined in `geo.hew`) and `area.hew` uses both
`Point` and `abs_int` (defined in `distance.hew`) without any `import`
statement — they are all part of the same `geo` module.

`main.hew` uses a selective import:

```hew
import geo::{Point, origin, manhattan, chebyshev, bounding_area};

let o = origin();
let p = Point { x: 3, y: 4 };
println(manhattan(o, p));        // 7
```

### Import style comparison

| Style | Syntax | Notes |
|-------|--------|-------|
| Bare | `import geo;` | All `pub` names in scope; also available as `geo.name(...)`. No false-positive warning when functions are called. |
| Selective | `import geo::{Point, manhattan}` | Only named symbols enter scope. Preferred when callers only need a subset. |
| Wildcard | `import geo::*;` | Same as bare, but currently emits a **false-positive "unused import" warning** when the import is consumed only through type references and no function call uses the `geo.name(...)` form. Prefer bare or selective to avoid the noise. |

Run it:
```sh
hew run   examples/multifile/02_geometry/main.hew
hew check examples/multifile/02_geometry/main.hew
```

Expected output:
```
origin  = (0, 0)
p       = (3, 4)
q       = (6, 8)
manhattan(o, p)   = 7
chebyshev(p, q)   = 4
bounding_area(o, q) = 48
```

---

## 03_text_stats — two-level module hierarchy

```
03_text_stats/
  text_stats/
    text_stats.hew           char_count, is_empty, truncate
    words/
      words.hew              word_count, first_word
  main.hew
```

**Pattern:** `text_stats/words/` is a **sub-module** of `text_stats`.
Its full import path is `text_stats::words`.

Importing the parent does **not** automatically expose the child:

```hew
import text_stats;           // gives text_stats.char_count, etc.
import text_stats::words;    // gives words.word_count, words.first_word, etc.
```

This means callers opt into sub-modules explicitly — a standard approach
for large packages where only a fraction of callers need the deeper API.

Run it:
```sh
hew run   examples/multifile/03_text_stats/main.hew
hew check examples/multifile/03_text_stats/main.hew
```

Expected output:
```
characters : 19
is_empty   : false
truncate   : the quick...
words      : 4
first word : the
```

---

## Known limitation — "unused import" false positive

The compiler's unused-import checker currently tracks whether an import is
**used as a qualified call** (`mod.func(...)`) but does **not** count
unqualified type construction or trait usage as evidence of use.  Concretely:

```hew
import shapes::{Circle};   // <-- warning: "unused import: shapes"
let c = Circle { ... };    //     even though Circle came from this import
```

**Workaround:** ensure at least one imported name is also called as a
function (`shapes.make_circle(...)`, or add the function to the selective
list).  Using a bare import (`import shapes;`) with a qualified call
(`shapes.some_fn(...)`) is the most reliable style until this is fixed.

This affects all three import forms: bare, selective, and wildcard.
The false positive is filed as a known issue.
