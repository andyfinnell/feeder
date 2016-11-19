-module(feeder_categories).

-export([get/2]).

-include("feeder_records.hrl").

get(term, C) ->
  C#category.term;
get(scheme, C) ->
  C#category.scheme;
get(label, C) ->
  C#category.label.

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

category_test() ->
  Category = #category{term = <<"cats">>, 
                  scheme = <<"https://google.com/schemes">>, 
                  label = <<"Cats">>},
  Term = feeder_categories:get(term, Category),
  ?assertMatch(Term, <<"cats">>),
  Scheme = feeder_categories:get(scheme, Category),
  ?assertMatch(Scheme, <<"https://google.com/schemes">>),
  Label = feeder_categories:get(label, Category),
  ?assertMatch(Label, <<"Cats">>),
  ok.
-endif.
