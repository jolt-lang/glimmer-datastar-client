# glimmer-datastar-client

A native thin client for [Datastar](https://data-star.dev)-style apps, on
[glimmer](https://github.com/jolt-lang/glimmer). The server owns the state and
the logic and streams the UI as data. The client is a terminal: it draws what
it is sent with whatever glimmer backend the app requires (GTK, the terminal,
UIKit, ...), keeps the page's signals, and sends user input back.

It is the client half of [glimmer-datastar](https://github.com/jolt-lang/glimmer-datastar),
which serves Datastar pages from jolt; any Datastar server that can send its
elements as data (EDN or JSON hiccup) works with it.

Status: early, but the loop works end to end: a page streamed over SSE, drawn
through glimmer, patched by `datastar-patch-elements` and
`datastar-patch-signals`, with actions posting signals back. Still to come: the
server's EDN mode in glimmer-datastar, demos on real toolkits, transitions (see
the beads epic `jds-s7a`).

```clojure
(require '[glimmer.core :as ui]
         '[glimmer-gtk.core]                  ; any glimmer backend
         '[jolt.datastar.client :as ds])

(def c (ds/client {:url "http://localhost:3000/" :root [:vbox {:id "app"}]}))
(ds/connect! c)                              ; GET :url, applying what it streams
(ui/run (fn [] (ds/view c)) :title "app")
```

## How a patch is applied

Each element with an `:id` is kept in its own cell and drawn by its own glimmer
component, keyed by that id; an unkeyed element is keyed by its tag and how many
of that tag precede it. So a patch to `#row-7` re-renders that row alone, and
glimmer's reconciler is the morph: an element keeps its widget, and what the
user typed into it, while its id (or tag and order) stays the same. All eight
modes are supported, plus `:data-ignore-morph` and `:data-preserve-attr`.
`replace` gives an element new widgets. Lifetime behaviours (`:data-init`,
intervals, computed signals...) start and stop with their element, parents
first. An element moved to a new parent keeps its signals and behaviours but
gets a new widget: glimmer does not reparent widgets.

Requests follow Datastar: a new request from an element cancels its last one,
and a failed request retries with backoff (`:retry :auto|:always|:never`,
honouring the server's `retry:`). Transport, UI thread, timers and the codec
are all options, so the client runs headless in tests.

## Elements

The server sends hiccup. Beside a backend's own props (`:label`, `:spacing`,
...), an element can carry Datastar's attributes as `:data-*` props, with EDN
expressions where Datastar has JavaScript:

```clojure
[:vbox {:id "counter" :data-signals {:count 0 :_open false}}
 [:label  {:id "n" :data-text (str "Count: " $count)}]
 [:button {:label "+1" :data-on {:click (swap! $count inc)}}]
 [:button {:label "save" :data-indicator :saving
           :data-attr {:sensitive (not $saving)}
           :data-on {:click (POST "/save")}}]
 [:entry  {:data-bind :name :data-on {:change {:do (GET "/search") :debounce 300}}}]
 [:label  {:data-show $_open :label "details"}]]
```

(Written in Clojure, that is quoted data: `'(swap! $count inc)`.)

Datastar's own attribute syntax works too, and is what a server written
against Datastar's SDKs produces; each attribute is read into the map form
when the element arrives (`jolt.datastar.client.syntax`):

```clojure
[:button {:id "save" :label "save"
          :data-on:click__debounce.300ms "(POST \"/save\")"
          :data-attr:sensitive "(not $saving)"
          :data-indicator:saving ""}]
[:entry  {:data-bind:first-name ""}]            ; $firstName
[:label  {:data-signals:count__ifmissing "0"
          :data-on-interval__duration.1s "(swap! $count inc)"}]
```

The rules are Datastar's: `__` separates modifiers, `:` the key, `.` a
modifier's tags; times are `300ms`, `2s` or a number; signal names are
camelCased unless `__case.kebab|snake|pascal` says otherwise; every value is an
expression. Browser-only modifiers (`__window`, `__outside`, `__prevent`,
`__stop`, ...) are accepted and ignored.

| attribute | does |
|---|---|
| `:data-signals {...}`, `:data-signals-if-missing {...}` | merge signals in |
| `:data-computed {:name expr}` | a signal derived from others |
| `:data-text expr` | the element's text (`:label`, an entry's `:text`) |
| `:data-show expr` | draw the element only while truthy |
| `:data-attr {:prop expr}` | any backend prop from an expression |
| `:data-bind :name` | two-way between the element's value and `$name` |
| `:data-on {:event expr}` | an action on an event; `evt` is its value. Modifiers: `{:do expr :debounce ms :throttle ms :delay ms :once true}` |
| `:data-on-interval expr` | every second, or `{:do expr :ms n}` |
| `:data-on-signal-patch expr` | after each server signal patch, or `{:do expr :include re}` |
| `:data-init expr`, `:data-effect expr` | once on appearing; now and on every change |
| `:data-indicator :name` | `$name` is true while this element's request runs |

## Expressions

EDN, interpreted. There is no interop, no eval and no I/O except the server
actions, so expressions stay what Datastar uses them for: presentation and
local behaviour. Application logic stays on the server.

- Reads: `$name`, `$a.b` (signals), `evt`, `el`, literals, vectors and maps.
- Forms: `if when when-not cond case do let and or`, `(peek $a)` to read
  without subscribing, and a fixed set of pure functions (arithmetic,
  comparison, strings, collections; see `jolt.datastar.client.expr/functions`).
- Actions, allowed only in `:data-on`, `:data-init`, `:data-effect`,
  `:data-on-interval` and `:data-on-signal-patch`:
  `(set! $a v)`, `(swap! $a f args...)`, `(toggle! $a)`,
  `(set-all! "regex" v)`, `(toggle-all! "regex")`,
  `(animate! $a {:to n :ms 300 :ease :in-out})`, and the server actions
  `(GET url opts?)`, `POST`, `PUT`, `PATCH`, `DELETE`, which send the signals
  (minus those with a segment starting with `_`, filtered further by
  `{:include re :exclude re}`).

Signals are fine-grained: each leaf is its own glimmer ratom, so a change
re-renders only the elements whose expressions read it.

## Wire protocol

Datastar's protocol, with data in place of HTML and JavaScript.

**Server to client** is Datastar's SSE framing unchanged: the event names
`datastar-patch-elements` and `datastar-patch-signals`, the `data:` line keys
(`selector`, `mode`, `elements`, `signals`, `onlyIfMissing`,
`useViewTransition`), and the `id:` and `retry:` fields. Only the `elements` and
`signals` values change: they are data, decoded by the codec (below).

```
event: datastar-patch-elements
data: selector #todo-list
data: mode inner
data: elements [:vbox {:id "todo-list"}
data: elements  [:label {:id "t1" :data-text (str $count " left")}]]

event: datastar-patch-signals
data: signals {:count 3 :form {:name nil}}
```

- `elements`: hiccup, one or more elements, over as many `data: elements`
  lines as needed (re-joined with newlines). An element's identity is its `:id`.
- `signals`: a map, merged by RFC 7386 rules (nil deletes).
- `selector`: `#id`, or none: with none, `outer` and `replace` target each
  element by its own id.
- `mode`: `outer` (default), `inner`, `replace`, `prepend`, `append`, `before`,
  `after`, `remove`. `namespace` is ignored.
- Retries follow Datastar's defaults: 1s, doubling, at most 30s, 10 times.
- A non-SSE response whose type is the codec's is a body of elements, patched
  `outer`, as Datastar treats `text/html`.

**Client to server**: an action sends `datastar-request: true` and the signals
(less `_`-prefixed ones and the action's filters), encoded by the codec: in the
`datastar` query parameter for GET, as the body otherwise, with
`Content-Type` and `Accept: text/event-stream, <type>` naming the codec's
content type. That header is how a server tells a native client from a browser.

**Data, and codecs.** The library works on data; reading and writing the wire
format is a codec's job, `{:content-type :encode :decode :decode-elements}`, so
the rest never parses. Two ship with it, both over parsers bundled with jolt:

- `codec/edn` (default): elements and signals as EDN.
- `codec/json`: what a Datastar server speaks. Signals go as a JSON body (or
  `?datastar=` for GET), which existing Datastar middleware such as
  glimmer-datastar's `wrap-signals` already parses; elements are JSON hiccup,
  `["vbox", {"id": "app"}, ["label", {"label": "hi"}]]`, with expressions as
  strings: `"data-on": {"click": "(swap! $count inc)"}`.

Signals are JSON-compatible EDN whatever the codec: keyword keys, and values
that are nil, booleans, numbers, strings, vectors or maps. A reply that is not
an event stream patches signals when its body is a map and elements otherwise,
as Datastar treats JSON and HTML replies.

An expression may always be a string of its syntax; that is how expressions
travel in JSON. A string is source code only at the top of an expression:
inside a form, `"text"` is a string.

## Examples

`examples/json_counter`: a JSON Datastar server (glimmer-datastar's middleware,
ring-chez) holding a count, and its thin client, which runs on GTK or in the
terminal from the same code.

```sh
jolt json-server            # http://localhost:3000/
jolt json-gtk               # or: jolt json-tui
```

## Toolkit independence

Nothing here names a toolkit. Which prop holds an element's text and value, and
which event reports a change, are tables a backend's vocabulary extends:

```clojure
(attrs/register-text-prop! :spinner :value)
(attrs/register-binding! :slider {:prop :value :event :on-change
                                  :value (fn [evt _current] evt)})
```

The client relies on glimmer 0.2: handlers that follow the latest render, props
diffed against the last render (`:patch-props!`) so an edit in progress is not
overwritten, and lists mixing keyed and unkeyed children.

## Running the tests

```sh
jolt test
```
