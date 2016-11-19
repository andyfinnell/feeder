-module(feeder_entries).

-export([get/2]).

-include("feeder_records.hrl").

get(authors, E) ->
  E#entry.authors;
get(duration, E) ->
  E#entry.duration;
get(id, E) ->
  E#entry.id;
get(image, E) ->
  E#entry.image;
get(links, E) ->
  E#entry.links;
get(published, E) ->
  feeder_utils:not_undefined(E#entry.published, E#entry.updated);
get(subtitle, E) ->
  E#entry.subtitle;
get(summary, E) ->
  E#entry.summary;
get(title, E) ->
  E#entry.title;
get(updated, E) ->
  feeder_utils:not_undefined(E#entry.updated, E#entry.published);
get(content, E) ->
  E#entry.content;
get(categories, E) ->
  E#entry.categories.
  
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

entry_test() ->
  Entry = #entry{
    authors = [#author{name = <<"Bob Jimbob">>}],
    duration = <<"59:00">>,
    id = <<"https://jimbob/bob/cats.html">>,
    image = <<"https://jimbob/bob/cats.jpg">>,
    links = [#link{url = <<"https://jimbob/bob/cats.html">>}],
    subtitle = #text{value = <<"Hello cats">>},
    summary = #text{value = <<"Words about cats">>},
    title = #text{value = <<"Cats">>},
    updated = <<"Sun, 04 Sep 2016 00:15:10 GMT">>,
    content = #text{value = <<"A lot of words about cats.">>},
    categories = [#category{term = <<"cats">>}]
  },
  Authors = feeder_entries:get(authors, Entry),
  ?assertMatch(Authors, [#author{name = <<"Bob Jimbob">>}]),
  Duration = feeder_entries:get(duration, Entry),
  ?assertMatch(Duration, <<"59:00">>),
  ID = feeder_entries:get(id, Entry),
  ?assertMatch(ID, <<"https://jimbob/bob/cats.html">>),  
  Image = feeder_entries:get(image, Entry),
  ?assertMatch(Image, <<"https://jimbob/bob/cats.jpg">>),
  Links = feeder_entries:get(links, Entry),
  ?assertMatch(Links, [#link{url = <<"https://jimbob/bob/cats.html">>}]),
  Subtitle = feeder_entries:get(subtitle, Entry),
  ?assertMatch(Subtitle, #text{value = <<"Hello cats">>}),
  Summary = feeder_entries:get(summary, Entry),
  ?assertMatch(Summary, #text{value = <<"Words about cats">>}),
  Title = feeder_entries:get(title, Entry),
  ?assertMatch(Title, #text{value = <<"Cats">>}),
  Updated = feeder_entries:get(updated, Entry),
  ?assertMatch(Updated, <<"Sun, 04 Sep 2016 00:15:10 GMT">>),
  Published = feeder_entries:get(published, Entry),
  ?assertMatch(Published, <<"Sun, 04 Sep 2016 00:15:10 GMT">>),
  Content = feeder_entries:get(content, Entry),
  ?assertMatch(Content, #text{value = <<"A lot of words about cats.">>}),
  Categories = feeder_entries:get(categories, Entry),
  ?assertMatch(Categories, [#category{term = <<"cats">>}]),
  ok.
  
entry_updated_test() ->
  Entry = #entry{
    updated = <<"Sun, 04 Sep 2016 00:15:10 GMT">>
  },
  Updated = feeder_entries:get(updated, Entry),
  ?assertMatch(Updated, <<"Sun, 04 Sep 2016 00:15:10 GMT">>),
  Published = feeder_entries:get(published, Entry),
  ?assertMatch(Published, <<"Sun, 04 Sep 2016 00:15:10 GMT">>),
  ok.
  
entry_published_test() ->
  Entry = #entry{
    published = <<"Sun, 04 Sep 2016 00:15:10 GMT">>
  },
  Updated = feeder_entries:get(updated, Entry),
  ?assertMatch(Updated, <<"Sun, 04 Sep 2016 00:15:10 GMT">>),
  Published = feeder_entries:get(published, Entry),
  ?assertMatch(Published, <<"Sun, 04 Sep 2016 00:15:10 GMT">>),
  ok.

entry_updated_published_test() ->
  Entry = #entry{
    updated = <<"Mon, 05 Sep 2016 00:15:10 GMT">>,
    published = <<"Sun, 04 Sep 2016 00:15:10 GMT">>
  },
  Updated = feeder_entries:get(updated, Entry),
  ?assertMatch(Updated, <<"Mon, 05 Sep 2016 00:15:10 GMT">>),
  Published = feeder_entries:get(published, Entry),
  ?assertMatch(Published, <<"Sun, 04 Sep 2016 00:15:10 GMT">>),
  ok.

-endif.
