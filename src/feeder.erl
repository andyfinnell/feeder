%% feeder - parse RSS and Atom formatted XML documents

-module(feeder).

-export([file/2]).
-export([stream/2]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include("feeder_records.hrl").

-record(element, {
  chars :: undefined | [binary()],
  attrs :: undefined | [tuple()]
}).
-type element() :: #element{}.

-record(state, {
  author :: undefined | author(),
  elements :: [element()],
  entry :: entry(),
  feed :: feed(),
  image :: boolean(),
  updatePeriod :: undefined | binary(),
  updateFrequency :: undefined | pos_integer(),
  user :: {user_state(), user_fun()}
}).

-type state() :: #state{}.
-type user_fun() :: function().
-type user_state() :: term().
-export_type([user_fun/0, user_state/0]).

%% State

push_element(State, Attrs) ->
  push_element(State, Attrs, []).
  
push_element(State, Attrs, Chars) ->
  Element = #element{chars = Chars, attrs = Attrs},
  State#state{elements = [Element | State#state.elements]}.
  
pop_element(State) ->
  State#state{elements = tl(State#state.elements)}.

concat_chars([], New) ->
  New;
concat_chars(Existing, []) ->  
  Existing;
concat_chars(Existing, New) ->
  [Existing | New].
  
append_chars(State, C) ->
  NewState = pop_element(State),
  push_element(NewState, attrs(State), concat_chars(chars(State), C)).

set_chars(State, C) ->
  NewState = pop_element(State),
  push_element(NewState, attrs(State), C).
  
chars(State) ->
  Element = hd(State#state.elements),
  Element#element.chars.
  
attrs(State) ->
  Element = hd(State#state.elements),
  Element#element.attrs.
  
%% Third pass

trim(S) when is_list(S) ->
  Bin = unicode:characters_to_binary(S, utf8),
  RE = "^[ \t\n\r]+|[ \t\n\r]+$",
  re:replace(Bin, RE, "", [global, {return, binary}]);
trim(S) ->
  S.

%% Once we deliberately set something, we do not override it.
update(T, F, L) when L =/= [] ->
  case element(F, T) of
    undefined -> setelement(F, T, trim(L));
    _ -> T
  end;
update(T, _, _) ->
  T.

prepend(T, F, V) when V =/= undefined ->
  NewList = case element(F, T) of
    undefined -> [V];
    List when is_list(List) -> [V | List]
  end,
  setelement(F, T, NewList);
prepend(T, _, _) ->
  T.

maybe_prepend(V, L, true) ->
  [V | L];
maybe_prepend(_V, L, false) ->
  L.
  
prepend_unique(T, F, V) when V =/= undefined ->
  NewList = case element(F, T) of
    undefined -> 
      [V];
    List when is_list(List) -> 
      maybe_prepend(V, List, not lists:member(V, List))
  end,
  setelement(F, T, NewList);
prepend_unique(T, _, _) ->
  T.

reduce_attrs(E, [H|T], Attr) ->
  {_, _, K, V} = H,
  reduce_attrs(Attr(E, list_to_atom(K), list_to_binary(V)), T, Attr);
reduce_attrs(E, [], _Attr) ->
  E.

text_attr(C, type, V) -> C#text{type=V};
text_attr(C, lang, V) -> C#text{language=V};
text_attr(C, _, _) -> C.

text(State) ->
  C = #text{value=trim(chars(State))},
  reduce_attrs(C, attrs(State), fun text_attr/3).

category_attr(C, term, V) -> C#category{term=V};
category_attr(C, scheme, V) -> C#category{scheme=V};
category_attr(C, label, V) -> C#category{label=V};
category_attr(C, text, V) -> C#category{term=V};
category_attr(C, _, _) -> C.

category(State) ->
  Cat = #category{term=trim(chars(State))},
  reduce_attrs(Cat, attrs(State), fun category_attr/3).

link_attr(L, href, V) -> L#link{url=V};
link_attr(L, length, V) -> L#link{length=binary_to_integer(V)};
link_attr(L, type, V) -> L#link{type=V};
link_attr(L, rel, V) -> L#link{rel=V};
link_attr(L, title, V) -> L#link{title=V};
link_attr(L, _, _) -> L.

link_entity(State) ->
  Link = #link{url=trim(chars(State))},
  reduce_attrs(Link, attrs(State), fun link_attr/3).
  
has_attr(L, Attr) ->
  lists:any(fun (Elem) ->
    {_, _, K, _} = Elem,
    list_to_atom(K) =:= Attr
  end, L).


empty_or_default([], DefaultValue) ->
  DefaultValue;
empty_or_default([Attr], _DefaultValue) ->
  {_, _, _, V} = Attr,
  V;
empty_or_default([Attr | _Tail], _DefaultValue) ->
  {_, _, _, V} = Attr,
  V.

get_attr(L, Attr, DefaultValue) ->
  Attrs = lists:filter(fun (Elem) ->
    {_, _, K, _} = Elem,
    list_to_atom(K) =:= Attr
  end, L),
  empty_or_default(Attrs, DefaultValue).

guid_link(Chars, true) ->
  #link{url = list_to_binary(Chars)};
guid_link(_Chars, false) ->
  undefined.

guid_link(State) ->
  IsPermalink = list_to_atom(get_attr(attrs(State), isPermaLink, "true")),
  guid_link(chars(State), IsPermalink).
  
enc_attr(E, url, V) -> E#link{url=V};
enc_attr(E, length, V) -> E#link{length=binary_to_integer(V)};
enc_attr(E, type, V) -> E#link{type=V};
enc_attr(E, _, _) -> E.

enclosure(State) ->
  reduce_attrs(#link{rel = <<"enclosure">>}, attrs(State), fun enc_attr/3).


content_link_attr(L, src, V) -> L#link{url=V};
content_link_attr(L, type, V) -> L#link{type=V};
content_link_attr(L, _, _) -> L.

content_link(State) ->
  reduce_attrs(#link{}, attrs(State), fun content_link_attr/3).

update_content(E, State, true) ->
  %% We have src attribute
  prepend(E, #entry.links, content_link(State));
update_content(E, State, _) ->
  %% Normal content
  update(E, #entry.content, text(State)).
  
-spec feed(feed(), atom(), state()) -> feed().
feed(F, author, State) ->
  prepend(F, #feed.authors, State#state.author);
feed(F, id, State) ->
  update(F, #feed.id, chars(State));
feed(F, guid, State) ->
  update(F, #feed.id, chars(State));
feed(F, url, State) when State#state.image =:= true ->
  update(F, #feed.image, chars(State));
feed(F, link, State) when State#state.image =:= true ->
  F; %% ignore it
feed(F, title, State) when State#state.image =:= true ->
  F; %% ignore it
feed(F, image, State) ->
  update(F, #feed.image, chars(State));
feed(F, language, State) ->
  update(F, #feed.language, chars(State));
feed(F, link, State) ->
  prepend(F, #feed.links, link_entity(State));
feed(F, subtitle, State) ->
  update(F, #feed.subtitle, text(State));
feed(F, summary, State) ->
  update(F, #feed.summary, text(State));
feed(F, title, State) ->
  update(F, #feed.title, text(State));
feed(F, category, State) ->
  prepend(F, #feed.categories, category(State));
feed(F, updated, State) ->
  update(F, #feed.updated, chars(State));
feed(F, published, State) ->
  update(F, #feed.published, chars(State));
feed(F, ttl, State) ->
  Chars = chars(State),
  {TTL, _} = string:to_integer(Chars),
  update(F, #feed.ttl, TTL);
feed(F, _, _) ->
  F.

-spec entry(entry(), atom(), state()) -> entry().
entry(E, author, State) ->
  prepend(E, #entry.authors, State#state.author);
entry(E, duration, State) ->
  update(E, #entry.duration, chars(State));
entry(E, enclosure, State) -> 
  prepend_unique(E, #entry.links, enclosure(State));
entry(E, id, State) ->
  update(E, #entry.id, chars(State));
entry(E, guid, State) ->
  Entry1 = update(E, #entry.id, chars(State)),
  Link = guid_link(State),
  prepend_unique(Entry1, #entry.links, Link);
entry(E, image, State) ->
  update(E, #entry.image, chars(State));
entry(E, link, State) ->
  prepend_unique(E, #entry.links, link_entity(State));
entry(E, published, State) ->
  update(E, #entry.published, chars(State));  
entry(E, subtitle, State) ->
  update(E, #entry.subtitle, text(State));
entry(E, summary, State) ->
  update(E, #entry.summary, text(State));
entry(E, content, State) ->
  update_content(E, State, has_attr(attrs(State), src));
entry(E, title, State) ->
  update(E, #entry.title, text(State));
entry(E, category, State) ->
  prepend(E, #entry.categories, category(State));
entry(E, updated, State) ->
  update(E, #entry.updated, chars(State));
entry(E, _, _) ->
  E.

-spec author(author(), atom(), state()) -> author().
author(A, name, State) ->
  update(A, #author.name, chars(State));
author(A, url, State) ->
  update(A, #author.url, chars(State));
author(A, email, State) ->
  update(A, #author.email, chars(State));
author(A, _, _) ->
  A.

%% Second pass

-define(IS_FEED,
  State#state.feed =/= undefined,
  State#state.entry =:= undefined).

-define(IS_ENTRY,
  State#state.entry =/= undefined).

-define(IS_AUTHOR,
  State#state.author =/= undefined).
  
href(Attrs) ->
  case lists:keyfind("href", 3, Attrs) of
    {_, _, "href", L} ->
      case lists:keyfind("rel", 3, Attrs) of
        {_, _, "rel", "shorturl"} -> [];
        _ -> L
      end;
    false -> []
  end.

-spec attribute(atom(), state(), atom(), list()) -> feed() | entry().
attribute(feed, State, image, Attrs) ->
  feed(State#state.feed, image, set_chars(State, href(Attrs)));
attribute(feed, State, _, _) ->
  State#state.feed;
attribute(entry, State, image, Attrs) ->
  entry(State#state.entry, image, set_chars(State, href(Attrs)));
attribute(entry, State, _, _) ->
  State#state.entry;
attribute(author, State, _, _) ->
  State#state.author.


flag(image, State, Flag) ->
  State#state{image=Flag};
flag(updatePeriod, State, false) when ?IS_FEED ->
  State#state{updatePeriod = list_to_atom(chars(State))};
flag(updateFrequency, State, false) when ?IS_FEED ->
  {Frequency, _} = string:to_integer(chars(State)),
  State#state{updateFrequency = Frequency};
flag(_, State, _) ->
  State.

-spec end_author(state()) -> state().
end_author(State) when ?IS_FEED ->
  Feed = feed(State#state.feed, author, State),  
  State#state{author=undefined, feed=Feed};
end_author(State) when ?IS_ENTRY ->
  Entry = entry(State#state.entry, author, State),  
  State#state{author=undefined, entry=Entry}.
  
safe_reverse(L) when is_list(L) ->
  lists:reverse(L);
safe_reverse(T) ->
  T.

minutes_in_period(hourly) ->
  60;
minutes_in_period(daily) ->
  24 * 60;
minutes_in_period(weekly) ->
  7 * 24 * 60;
minutes_in_period(monthly) ->
  30 * 24 * 60;
minutes_in_period(yearly) ->
  52 * 7 * 24 * 60.

update_update(F, undefined, undefined) ->
  F;
update_update(F, undefined, UpdateFrequency) ->
  update_update(F, daily, UpdateFrequency);
update_update(F, UpdatePeriod, undefined) ->
  update_update(F, UpdatePeriod, 1);
update_update(F, UpdatePeriod, UpdateFrequency) ->
  TTL = minutes_in_period(UpdatePeriod) div UpdateFrequency,
  F#feed{ttl = TTL}.
  
-spec end_element(atom(), state()) -> state().
end_element(undefined, State) ->
  pop_element(State);
end_element(document, State) ->
  {UserState, UserFun} = State#state.user,
  UserFun(endFeed, UserState);
end_element(feed, State) ->
  Feed1 = update_update(State#state.feed, State#state.updatePeriod, State#state.updateFrequency),  
  Feed2 = Feed1#feed{authors = safe_reverse(Feed1#feed.authors),
                    links = safe_reverse(Feed1#feed.links),
                    categories = safe_reverse(Feed1#feed.categories)},
  {UserState, UserFun} = State#state.user,
  NewUserState = UserFun({feed, Feed2}, UserState),
  pop_element(State#state{feed=undefined, user={NewUserState, UserFun}});
end_element(entry, State) ->
  Entry1 = State#state.entry,
  Entry2 = Entry1#entry{authors = safe_reverse(Entry1#entry.authors),
                        links = safe_reverse(Entry1#entry.links),
                        categories = safe_reverse(Entry1#entry.categories)},
  {UserState, UserFun} = State#state.user,
  NewUserState = UserFun({entry, Entry2}, UserState),
  pop_element(State#state{entry=undefined, user={NewUserState, UserFun}});
end_element(author, State) ->
  Author = author(State#state.author, name, State), %% might just be chars
  NewState = State#state{author=Author},  
  pop_element(end_author(NewState));
end_element(E, State) when ?IS_AUTHOR ->
  NewState = flag(E, State, false),
  Author = author(NewState#state.author, E, State),
  pop_element(NewState#state{author=Author});
end_element(E, State) when ?IS_FEED ->
  NewState = flag(E, State, false),
  Feed = feed(NewState#state.feed, E, State),
  pop_element(NewState#state{feed=Feed});
end_element(E, State) when ?IS_ENTRY ->
  NewState = flag(E, State, false),
  Entry = entry(NewState#state.entry, E, State),
  pop_element(NewState#state{entry=Entry}).

-spec start_element(atom(), list(), state()) -> state().
start_element(undefined, Attrs, State) ->
  push_element(State, Attrs);
start_element(feed, Attrs, State) ->
  push_element(State#state{feed=#feed{}}, Attrs);
start_element(entry, Attrs, State) ->
  push_element(State#state{entry=#entry{}}, Attrs);
start_element(author, Attrs, State) ->
  push_element(State#state{author=#author{}}, Attrs);
start_element(E, Attrs, State) when ?IS_AUTHOR ->
  NewState = flag(E, State, true),
  Author = attribute(author, NewState, E, Attrs),
  push_element(NewState#state{author=Author}, Attrs);
start_element(E, Attrs, State) when ?IS_FEED ->
  NewState = flag(E, State, true),
  Feed = attribute(feed, NewState, E, Attrs),
  push_element(NewState#state{feed=Feed}, Attrs);
start_element(E, Attrs, State) when ?IS_ENTRY ->
  NewState = flag(E, State, true),
  Entry = attribute(entry, NewState, E, Attrs),
  push_element(NewState#state{entry=Entry}, Attrs).

%% First pass

qname({_, "author"}) -> author;
qname({_, "channel"}) -> feed;
qname({_, "contributor"}) -> author;
qname({_, "creator"}) -> author;
qname({_, "date"}) -> updated;
qname({_, "description"}) -> summary;
qname({_, "duration"}) -> duration;
qname({_, "enclosure"}) -> enclosure;
qname({_, "entry"}) -> entry;
qname({_, "feed"}) -> feed;
qname({_, "guid"}) -> guid;
qname({_, "id"}) -> id;
qname({_, "identifier"}) -> id;
qname({_, "image"}) -> image;
qname({_, "item"}) -> entry;
qname({_, "language"}) -> language;
qname({_, "link"}) -> link;
qname({_, "name"}) -> name;
qname({_, "pubDate"}) -> published;
qname({_, "published"}) -> published;
qname({_, "subtitle"}) -> subtitle;
qname({_, "summary"}) -> summary;
qname({_, "title"}) -> title;
qname({_, "updated"}) -> updated;
qname({_, "lastBuildDate"}) -> updated;
qname({_, "ttl"}) -> ttl;
qname({_, "url"}) -> url;
qname({_, "uri"}) -> url;
qname({_, "updatePeriod"}) -> updatePeriod;
qname({_, "updateFrequency"}) -> updateFrequency;
qname({_, "email"}) -> email;
qname({_, "category"}) -> category;
qname({"media", "content"}) -> undefined;
qname({_, "content"}) -> content;
qname({"content", "encoded"}) -> content;
qname({_, _}) -> undefined.

event(startDocument, _, S) ->
  S;
event({startElement, _, _LocalName, QName, Attrs}, _, S) ->
  start_element(qname(QName), Attrs, S);
event({endElement, _, _LocalName, QName}, _, S) ->
  end_element(qname(QName), S);
event({characters, C}, _, S) ->
  append_chars(S, C);
event(endDocument, _, S) ->
  end_element(document, S);
event(_, _, S) ->
  S.

%% API

event({entry, Entry}, {Feed, Entries}) ->
  {Feed, [Entry|Entries]};
event({feed, Feed}, {[], Entries}) ->
  {Feed, Entries};
event(endFeed, {Feed, Entries}) ->
  {Feed, lists:reverse(Entries)}.

user_state(undefined) -> {[], []};
user_state(S) -> S.

user_fun(undefined) -> fun event/2;
user_fun(F) -> F.

opts(file, Opts) ->
  US = user_state(proplists:get_value(event_state, Opts)),
  UF = user_fun(proplists:get_value(event_fun, Opts)),
  User = {US, UF},
  [{event_state, #state{user=User}}, {event_fun, fun event/3}];
opts(stream, Opts) ->
  CS = proplists:get_value(continuation_state, Opts),
  CF = proplists:get_value(continuation_fun, Opts),
  [{continuation_state, CS}, {continuation_fun, CF}] ++ opts(file, Opts).

-spec file(binary() | maybe_improper_list(), [any()]) -> any().
file(Filename, Opts) ->
  xmerl_sax_parser:file(Filename, opts(file, Opts)).

-spec stream(binary() | maybe_improper_list(), [any()]) -> any().
stream(Xml, Opts) ->
  xmerl_sax_parser:stream(Xml, opts(stream, Opts)).

-ifdef(TEST).
trim_test() ->
  ?assertMatch(<<"">>, trim("")),
  ?assertMatch(<<"hello">>, trim(" hello ")),
  ok.

q([{Wanted, Names}|T]) ->
  F = fun (Name) -> ?assertMatch(Wanted, qname({"", Name})) end,
  lists:map(F, Names),
  q(T);
q([]) ->
  ok.
qname_test() -> q([
  {author, ["author"]},
  {duration, ["duration"]},
  {enclosure, ["enclosure"]},
  {entry, ["entry", "item"]},
  {feed, ["feed", "channel"]},
  {id, ["id"]},
  {image, ["image"]},
  {language, ["language"]},
  {link, ["link"]},
  {name, ["name"]},
  {published, ["pubDate", "published"]},
  {subtitle, ["subtitle"]},
  {summary, ["summary", "description"]},
  {title, ["title"]},
  {undefined, ["wtf", "", "!"]},
  {updated, ["updated"]}
]).
-endif.
