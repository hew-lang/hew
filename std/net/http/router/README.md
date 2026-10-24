# std.net.http.router

A pure Hew route table shared by HTTP transports. Store ordinary typed route
values and dispatch them with `match`; the router owns no requests or connections.

```hew
import std.net.http.router;

enum Endpoint { User; NewUser; }

fn main() {
    var routes: router.Router<Endpoint> = router.new();
    routes.add("GET", "/users/{id}", .User).expect("valid route");
    routes.add("POST", "/users/new", .NewUser).expect("valid route");

    let found = routes.resolve("GET", "/users/42?view=full").expect("user route");
    println(found.param_i64("id").expect("integer ID"));
}
```

`add` returns `Result<(), RouteConfigError>`. A parameter occupies a whole
segment and has a unique ASCII identifier. The same method and path shape may
be registered only once: `/users/{id}` and `/users/{name}` are duplicates.
Different methods may give that parameter different names. A failed addition
leaves the table intact.

`resolve(method, target)` returns `Result<RouteMatch<T>, RouteError>`. Pass the
method and raw target from `request.method()` and `request.path()` in either
`std.net.http` or `std.net.http.codec`. Among complete path matches,
the first differing segment prefers a literal to a parameter, regardless of
registration order. Path selection comes before method selection: a `GET` for
`/users/new` above returns `MethodNotAllowed(["POST"])`, even though the parameter
route accepts `GET`. A missing path returns `NotFound`. Method names are
case-sensitive; register `HEAD` and `OPTIONS` explicitly when needed.

The target must begin with `/`. The query is ignored, fragments and control
characters are rejected, and raw spaces must be escaped. Literal patterns use
URL percent encoding too. Both paths and patterns split on raw `/` **before**
strict decoding through `std.net.url.decode`. Each component is decoded once:
`/users/a%2Fb` captures `a/b` as one parameter; `%252F` captures `%2F`; a literal
`+` stays `+`. Encoded backslashes are ordinary content too. No case, dot-segment,
repeated-slash or trailing-slash normalization occurs. An empty segment cannot
match a parameter. Treat captures as data; routing does not make them safe
filesystem paths.

`RouteMatch.value` is the selected typed route value. `param(name)` returns a
decoded string; `param_i64(name)` checks the entire signed decimal value and its
range, and `param_bool(name)` accepts exactly `true` or `false`. These methods
return `Result<_, ExtractError>`, distinguishing a missing parameter from invalid
data. For other types, pass `param(name)?` to the application's parser. Do not
decode the returned text a second time.

See [the complete dispatch example](../../../../examples/http_router.hew), which
handles valid and invalid IDs, 404, and 405 with its available-method metadata.
The transport writes those methods as an `Allow` header. The table uses a linear
scan and ordinary Hew collections; it adds no middleware or handler framework.
