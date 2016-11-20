-module(feeder_entries).

-export([get/2]).

-include("feeder_records.hrl").

get(authors, E) ->
  E#entry.authors;
get(categories, E) ->
  E#entry.categories;
get(content, E) ->
  E#entry.content;
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
get(unique_id, E) ->
  first_not_undefined([
    fun () -> get(id, E) end,
    fun () -> unique_link(E) end,
    fun () -> hash_properties(E) end
  ]);
get(updated, E) ->
  feeder_utils:not_undefined(E#entry.updated, E#entry.published).
  

first_not_undefined(ListOfFun) ->
  first_not_undefined(ListOfFun, undefined).

first_not_undefined([], undefined) ->
  undefined;
first_not_undefined([Next | Rest], undefined) ->
  first_not_undefined(Rest, Next());
first_not_undefined(_ListOfFun, Value) ->
  Value.

is_link_unique(Link) ->
  %% What constitutes a unique link?
  case feeder_links:get(rel, Link) of
    <<"related">> -> false;
    <<"via">> -> false;
    _ -> true
  end.

is_link_more_unique(Link1, Link2) ->
  %% rel ordering: self, alternate, undefined, enclosure
  Rel1 = feeder_links:get(rel, Link1),
  Rel2 = feeder_links:get(rel, Link2),
  case {Rel1, Rel2} of
    {<<"self">>, _} -> true;
    {_, <<"self">>} -> false;
    {<<"alternate">>, _} -> true;
    {_, <<"alternate">>} -> false;
    {undefined, _} -> true;
    {_, undefined} -> false;
    {_, _} -> true %% no idea, so keep order
  end.
  
unique_link(E) ->
  PotentialLinks = lists:filter(fun is_link_unique/1, E#entry.links),
  SortedLinks = lists:sort(fun is_link_more_unique/2, PotentialLinks),
  case SortedLinks of 
    [] -> undefined;
    [Link | _Rest] -> Link#link.url
  end.

hash(IOData) ->
  <<BigNum:256/big-unsigned-integer>> = crypto:hash(sha256, IOData),
  list_to_binary(io_lib:format("~64.16.0b", [BigNum])).

prepend_text(undefined, Output) ->
  Output;
prepend_text(Text, Output) ->
  [Text | Output].

concat_text([], Output) ->
  lists:reverse(Output);
concat_text([undefined | Rest], Output) ->
  concat_text(Rest, Output);
concat_text([Text | Rest], Output) ->
  concat_text(Rest, prepend_text(feeder_text:get(value, Text), Output)).
  
hash_properties(E) ->
  %% Hash: title ++ subtitle ++ summary ++ content
  Data = concat_text([
    feeder_entries:get(title, E),
    feeder_entries:get(subtitle, E),
    feeder_entries:get(summary, E),
    feeder_entries:get(content, E)
  ], []),
  hash(Data).
  
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
  
entry_has_id_test() ->
  Entry = #entry{
    id = <<"uniqueness">>,
    links = [#link{url = <<"https://jimbob/bob/cats.html">>}],
    title = #text{value = <<"Hello cats">>},
    content = #text{value = <<"Words about cats">>}
  },
  ID = feeder_entries:get(unique_id, Entry),
  ?assertMatch(ID, <<"uniqueness">>),
  ok.

entry_no_id_but_link_test() ->
  Entry = #entry{
    links = [#link{url = <<"https://jimbob/bob/cats.html">>}],
    title = #text{value = <<"Hello cats">>},
    content = #text{value = <<"Words about cats">>}
  },
  ID = feeder_entries:get(unique_id, Entry),
  ?assertMatch(ID, <<"https://jimbob/bob/cats.html">>),
  ok.

entry_prefer_self_for_id_test() ->
  Entry = #entry{
    links = [#link{url = <<"https://jimbob/bob/kitteh.html">>, rel = <<"alternate">>},
            #link{url = <<"https://jimbob/bob/cats.html">>},
            #link{url = <<"https://jimbob/bob/fuzzy.html">>, rel = <<"self">>}],
    title = #text{value = <<"Hello cats">>},
    content = #text{value = <<"Words about cats">>}
  },
  ID = feeder_entries:get(unique_id, Entry),
  ?assertMatch(ID, <<"https://jimbob/bob/fuzzy.html">>),
  ok.

entry_prefer_alternate_for_id_test() ->
  Entry = #entry{
    links = [#link{url = <<"https://jimbob/bob/kitteh.html">>, rel = <<"alternate">>},
            #link{url = <<"https://jimbob/bob/cats.html">>},
            #link{url = <<"https://jimbob/bob/fuzzy.html">>, rel = <<"via">>}],
    title = #text{value = <<"Hello cats">>},
    content = #text{value = <<"Words about cats">>}
  },
  ID = feeder_entries:get(unique_id, Entry),
  ?assertMatch(ID, <<"https://jimbob/bob/kitteh.html">>),
  ok.

entry_prefer_unadorned_for_id_test() ->
  Entry = #entry{
    links = [#link{url = <<"https://jimbob/bob/kitteh.html">>, rel = <<"enclosure">>},
            #link{url = <<"https://jimbob/bob/cats.html">>},
            #link{url = <<"https://jimbob/bob/fuzzy.html">>, rel = <<"via">>}],
    title = #text{value = <<"Hello cats">>},
    content = #text{value = <<"Words about cats">>}
  },
  ID = feeder_entries:get(unique_id, Entry),
  ?assertMatch(ID, <<"https://jimbob/bob/cats.html">>),
  ok.

entry_use_hash_for_id_test() ->
  Entry = #entry{
    links = [#link{url = <<"https://jimbob/bob/fuzzy.html">>, rel = <<"via">>}],
    title = #text{value = <<"Hello cats">>},
    content = #text{value = <<"Words about cats">>}
  },
  ID = feeder_entries:get(unique_id, Entry),
  io:format("Want = ~p~nFound = ~p~n~n ", [<<"d95912cc811ab50d7bed3a05d8cc76c94a4a54d5641a534a5e609301d1595711">>, ID]),  
  ?assertMatch(ID, <<"d95912cc811ab50d7bed3a05d8cc76c94a4a54d5641a534a5e609301d1595711">>),
  ok.

-endif.
