-module(feeder_links).

-export([get/2]).

-include("feeder_records.hrl").

get(url, L) ->
  L#link.url;
get(length, L) ->
  L#link.length;
get(type, L) ->
  L#link.type;
get(rel, L) ->
  L#link.rel;
get(title, L) ->
  L#link.title.

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

link_test() ->
  Link = #link{ 
    url = <<"https://jimbob.com/bob/pounce.mp4">>,
    length = <<"6234876">>,
    type = <<"video/mpeg">>,
    rel = <<"enclosure">>,
    title = <<"Pouncing">>
  },
  URL = feeder_links:get(url, Link),
  ?assertMatch(URL, <<"https://jimbob.com/bob/pounce.mp4">>),
  Length = feeder_links:get(length, Link),
  ?assertMatch(Length, <<"6234876">>),
  Type = feeder_links:get(type, Link),
  ?assertMatch(Type, <<"video/mpeg">>),
  Rel = feeder_links:get(rel, Link),
  ?assertMatch(Rel, <<"enclosure">>),
  Title = feeder_links:get(title, Link),
  ?assertMatch(Title, <<"Pouncing">>),
  ok.
-endif.
