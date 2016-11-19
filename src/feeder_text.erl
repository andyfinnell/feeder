-module(feeder_text).

-export([get/2]).

-include("feeder_records.hrl").

get(type, C) ->
  C#text.type;
get(value, C) ->
  C#text.value;
get(language, C) ->
  C#text.language.

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

text_test() ->
  Text = #text{
    value = <<"Stuff goes in here">>, 
    type = <<"text">>, 
    language = <<"en-us">>
  },
  Value = feeder_text:get(value, Text),
  ?assertMatch(Value, <<"Stuff goes in here">>),
  Language = feeder_text:get(language, Text),
  ?assertMatch(Language, <<"en-us">>),
  Type = feeder_text:get(type, Text),
  ?assertMatch(Type, <<"text">>),
  ok.
-endif.
