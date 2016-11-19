-module(feeder_authors).

-export([get/2]).

-include("feeder_records.hrl").

get(name, A) ->
  A#author.name;
get(url, A) ->
  A#author.url;
get(email, A) ->
  A#author.email.

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

author_test() ->
  Author = #author{name = <<"Bob Jimbob">>, 
                  email = <<"bob@jimbob.com">>, 
                  url = <<"https://jimbob.com/bob">>},
  Name = feeder_authors:get(name, Author),
  ?assertMatch(Name, <<"Bob Jimbob">>),
  URL = feeder_authors:get(url, Author),
  ?assertMatch(URL, <<"https://jimbob.com/bob">>),
  Email = feeder_authors:get(email, Author),
  ?assertMatch(Email, <<"bob@jimbob.com">>),
  ok.
-endif.
