-module(feeder_feeds).

-export([get/2]).

-include("feeder_records.hrl").

get(authors, F) ->
  F#feed.authors;
get(id, F) ->
  F#feed.id;
get(image, F) ->
  F#feed.image;
get(language, F) ->
  F#feed.language;
get(links, F) ->
  F#feed.links;
get(published, F) ->
  feeder_utils:not_undefined(F#feed.published, F#feed.updated);
get(subtitle, F) ->
  F#feed.subtitle;
get(summary, F) ->
  F#feed.summary;
get(title, F) ->
  F#feed.title;
get(updated, F) ->
  feeder_utils:not_undefined(F#feed.updated, F#feed.published);
get(ttl, F) ->
  F#feed.ttl;
get(categories, F) ->
  F#feed.categories.

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

feed_test() ->
  Feed = #feed{
    authors = [#author{name = <<"Bob Jimbob">>}],
    id = <<"https://jimbob.com/bob">>,
    image = <<"https://jimbob.com/bob.jpg">>,
    language = <<"en-us">>,
    links = [#link{url = <<"https://jimbob.com/bob">>}],
    subtitle = #text{value = <<"Deep thoughts">>},
    summary = #text{value = <<"Thoughts of secret agent Bob Jimbob.">>},
    title = #text{value = <<"Secret Agent Cat">>},
    updated = <<"Sun, 04 Sep 2016 00:15:10 GMT">>,
    ttl = 60,
    categories = [#category{term = <<"cats">>}]
  },
  Author = feeder_feeds:get(authors, Feed),
  ?assertMatch(Author, [#author{name = <<"Bob Jimbob">>}]),
  ID = feeder_feeds:get(id, Feed),
  ?assertMatch(ID, <<"https://jimbob.com/bob">>),
  Image = feeder_feeds:get(image, Feed),
  ?assertMatch(Image, <<"https://jimbob.com/bob.jpg">>),
  Language = feeder_feeds:get(language, Feed),
  ?assertMatch(Language, <<"en-us">>),
  Links = feeder_feeds:get(links, Feed),
  ?assertMatch(Links, [#link{url = <<"https://jimbob.com/bob">>}]),
  Subtitle = feeder_feeds:get(subtitle, Feed),
  ?assertMatch(Subtitle, #text{value = <<"Deep thoughts">>}),
  Summary = feeder_feeds:get(summary, Feed),
  ?assertMatch(Summary, #text{value = <<"Thoughts of secret agent Bob Jimbob.">>}),
  Title = feeder_feeds:get(title, Feed),
  ?assertMatch(Title, #text{value = <<"Secret Agent Cat">>}),
  Published = feeder_feeds:get(published, Feed),
  ?assertMatch(Published, <<"Sun, 04 Sep 2016 00:15:10 GMT">>),
  Updated = feeder_feeds:get(updated, Feed),
  ?assertMatch(Updated, <<"Sun, 04 Sep 2016 00:15:10 GMT">>),
  TTL = feeder_feeds:get(ttl, Feed),
  ?assertMatch(TTL, 60),
  Categories = feeder_feeds:get(categories, Feed),
  ?assertMatch(Categories, [#category{term = <<"cats">>}]),
  ok.
  
feed_update_test() ->
  %% If have updated, but no published, published should return updated
  Feed = #feed{
    updated = <<"Sun, 04 Sep 2016 00:15:10 GMT">>
  },
  Published = feeder_feeds:get(published, Feed),
  ?assertMatch(Published, <<"Sun, 04 Sep 2016 00:15:10 GMT">>),
  Updated = feeder_feeds:get(updated, Feed),
  ?assertMatch(Updated, <<"Sun, 04 Sep 2016 00:15:10 GMT">>),
  ok.

feed_published_test() ->
  %% If have published, but no updated, updated should return published
  Feed = #feed{
    published = <<"Sun, 04 Sep 2016 00:15:10 GMT">>
  },
  Published = feeder_feeds:get(published, Feed),
  ?assertMatch(Published, <<"Sun, 04 Sep 2016 00:15:10 GMT">>),
  Updated = feeder_feeds:get(updated, Feed),
  ?assertMatch(Updated, <<"Sun, 04 Sep 2016 00:15:10 GMT">>),
  ok.

feed_update_published_test() ->
  %% If have published and updated, each should be returned
  Feed = #feed{
    updated = <<"Mon, 05 Sep 2016 00:15:10 GMT">>,
    published = <<"Sun, 04 Sep 2016 00:15:10 GMT">>
  },
  Published = feeder_feeds:get(published, Feed),
  ?assertMatch(Published, <<"Sun, 04 Sep 2016 00:15:10 GMT">>),
  Updated = feeder_feeds:get(updated, Feed),
  ?assertMatch(Updated, <<"Mon, 05 Sep 2016 00:15:10 GMT">>),
  ok.

-endif.
