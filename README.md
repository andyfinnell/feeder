# feeder - parse RSS and Atom

The **feeder** [Erlang](http://www.erlang.org/) library parses RSS and Atom formatted XML feeds. It is a stream based parser that sends its events through a callback interface.

## Dependencies

**feeder** depends on [rebar3](http://www.rebar3.org/) to build.

## Usage

Parse a file and accumulate parser events:

```erlang
-module(acc).
-export([file/1]).

event({entry, Entry}, {Feed, Entries}) ->
  {Feed, [Entry|Entries]};
event({feed, Feed}, {[], Entries}) ->
  {Feed, Entries};
event(endFeed, {Feed, Entries}) ->
  {Feed, lists:reverse(Entries)}.

opts() ->
  [{event_state, {[],[]}}, {event_fun, fun event/2}].

file(Filename) ->
  {ok, EventState, _Rest} = feeder:file(Filename, opts()),
  EventState.
```

## Example

To try HTTP streaming do:

```
cd example
rebar3 compile
erl -pa ebin deps/*/ebin
```

```erlang
example:start().
example:print_titles("http://5by5.tv/rss").
```

## Types

### author()

An author or contributor to a `feed` or `entry`.

```erlang
feeder_authors:get(Key, Author) -> Value
```
- `Key = name | url | email`
- `Author = author()`
- `Value = binary() | undefined`

### category()

A tag used to categorize a `feed` or `entry`.

```erlang
feeder_categories:get(Key, Category) -> Value
```
- `Key = term | scheme | label`
- `Category = category()`
- `Value = binary() | undefined`

### entry()

An `item` or `entry` tuple.

```erlang
feeder_entries:get(Key, Entry) -> Value
```
- `Key = authors | categories | content | duration | id | image | links | published | subtitle | summary | title | updated`
- `Entry = entry()`
- `Value = binary() | [author()] | [category()] | text() | [link()] | undefined`

| Key | Type |
| --- | --- |
| `authors` | `[author()]` |
| `categories` | `[category()]` |
| `content` | `text()` |
| `duration` | `binary()` |
| `id` | `binary()` |
| `image` | `binary()` |
| `links` | `[link()]` |
| `published` | `binary()` |
| `subtitle` | `text()` |
| `summary` | `text()` |
| `title` | `text()` |
| `updated` | `binary()` |


### feed()

The `channel` or `feed` tuple.

```erlang
feeder_feeds:get(Key, Feed) -> Value
```
- `Key = authors | categories | id | image | language | links | published | subtitle | summary | title | ttl | updated`
- `Feed = feed()`
- `Value = [author()] | [category()] | binary() | [link()] | text() | pos_integer() | undefined`

| Key | Type |
| --- | --- |
| `authors` | `[author()]` |
| `categories` | `[category()]` |
| `id` | `binary()` |
| `image` | `binary()` |
| `language` | `binary()` |
| `links` | `[link()]` |
| `published` | `binary()` |
| `subtitle` | `text()` |
| `summary` | `text()` |
| `title` | `text()` |
| `ttl` | `pos_integer()` |
| `updated` | `binary()` |

### link()

A link contained by a `feed` or `entry`. It can include enclosures.

```erlang
feeder_links:get(Key, Link) -> Value
```
- `Key = length | url | type | rel | title`
- `Link = link()`
- `Value = binary() | undefined`

### text()

A chunk of text optionally decorated by language and type. It used by titles, subtitles, summaries, and content.

```erlang
feeder_text:get(Key, Text) -> Value
```
- `Key = value | type | language`
- `Text = text()`
- `Value = binary() | undefined`

### option()

Options to setup the parser.

`{continuation_fun, ContinuationFun}`
[`ContinuationFun`](http://www.erlang.org/doc/man/xmerl_sax_parser.html#ContinuationFun-1) is a call back function to decide what to do if the parser runs into EOF before the document is complete.

`{continuation_state, term()}`
State that is accessible in the continuation call back function.

`{event_fun, EventFun}`
[`EventFun`](http://www.erlang.org/doc/man/xmerl_sax_parser.html#EventFun-3) is the call back function for parser events.

`{event_state, term()}`
State that is accessible in the event call back function.

### event()

The events that are sent to the user via the callback.

```erlang
{feed, Feed}
```

- `Feed = feed()`

Receive notification when the meta information of the feed or channel has been parsed.

```erlang
{entry, Entry}
```

- `Entry = entry()`

Receive notification for each entry or article in the feed.

```erlang
endFeed
```

Receive notification of the end of a document. **feeder** will send this event only once, and it will be the last event during the parse.

## Tasks

### Parsing feeds

```erlang
feeder:file(Filename, Opts) -> Result
```
- `Filename = string()`
- `Opts = [option()]`
- `Result = {ok, EventState, Rest}`
- `Rest = unicode_binary() | latin1_binary()`
- `EventState = term()`

```erlang
feeder:stream(Xml, Opts) -> Result
```
- `Xml = unicode_binary() | latin1_binary() | [unicode_char()]`
- `Opts = [option()]`
- `Result = {ok, EventState, Rest}`
- `Rest = unicode_binary() | latin1_binary()`
- `EventState = term()`

## License

[MIT License](https://raw.github.com/andyfinnell/feeder/master/LICENSE)
