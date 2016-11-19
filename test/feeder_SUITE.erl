-module(feeder_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([all/0]).
-export([suite/0]).
-export([groups/0]).
-export([init_per_group/2]).
-export([end_per_group/2]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

-export([atom/1]).
-export([itunes/1]).
-export([rss/1]).
-export([author/1]).
-export([aaronvegh/1]).
-export([applenews/1]).
-export([bleacherreport/1]).
-export([cabel/1]).
-export([globnerdishness/1]).
-export([cnn/1]).
-export([five12pixels/1]).
-export([gratefulDead/1]).
-export([rssTwoExample2/1]).
-export([sampleRss/1]).
-export([dublincore/1]).
-export([measure_time/1]).

all() -> [
  {group, atom},
  {group, rss},
  {group, itunes},
  measure_time
].

suite() ->
  [{timetrap, {seconds, 60}}].

groups() -> [
  {atom, [parallel], [atom, author, cabel, globnerdishness]},
  {rss, [parallel], [rss, aaronvegh, applenews, bleacherreport, cnn, five12pixels, gratefulDead, rssTwoExample2, sampleRss, dublincore]},
  {itunes, [parallel], [itunes]}
].

init_per_group(_, Config) ->
  Config.

end_per_group(_, _) ->
  ok.

init_per_testcase(measure_time, _) ->
  {skip, optional};
init_per_testcase(_, Config) ->
  Config.

end_per_testcase(_, _) ->
  ok.

event({entry, Entry}, {Feed, Entries}) ->
  {Feed, [Entry|Entries]};
event({feed, Feed}, {[], Entries}) ->
  {Feed, Entries};
event(endFeed, {Feed, Entries}) ->
  {Feed, lists:reverse(Entries)}.

opts() ->
  [{event_state, {[],[]}}, {event_fun, fun event/2}].

parse(Filename) ->
  {ok, EventState, _Rest} = feeder:file(Filename, opts()),
  EventState.

name(Conf, Name) ->
  Dir = proplists:get_value(data_dir, Conf),
  [filename:join([Dir, Name])|".xml"].

test(Conf, Name, Wanted) ->
  Filename = name(Conf, Name),
  Found = parse(Filename),
  {WantedFeed, WantedEntries} = Wanted,
  {FoundFeed, FoundEntries} = Found,  
  WantedFeed = FoundFeed,
  Entries = lists:zip(WantedEntries, FoundEntries),
  lists:foreach(fun ({WantedEntry, FoundEntry}) ->
    WantedEntry = FoundEntry
  end, Entries),
  Wanted = Found,
  ok.

atom(Conf) -> test(Conf, "atom", atom:wanted()).
author(Conf) -> test(Conf, "author", author:wanted()).
cabel(Conf) -> test(Conf, "cabel", cabel:wanted()).
globnerdishness(Conf) -> test(Conf, "globnerdishness", globnerdishness:wanted()).
rss(Conf) -> test(Conf, "rss", rss:wanted()).
aaronvegh(Conf) -> test(Conf, "aaronvegh", aaronvegh:wanted()).
applenews(Conf) -> test(Conf, "applenews", applenews:wanted()).
bleacherreport(Conf) -> test(Conf, "bleacherreport", bleacherreport:wanted()).
cnn(Conf) -> test(Conf, "cnn", cnn:wanted()).
five12pixels(Conf) -> test(Conf, "five12pixels", five12pixels:wanted()).
gratefulDead(Conf) -> test(Conf, "gratefulDead", gratefulDead:wanted()).
rssTwoExample2(Conf) -> test(Conf, "rssTwoExample2", rssTwoExample2:wanted()).
sampleRss(Conf) -> test(Conf, "sampleRss", sampleRss:wanted()).
dublincore(Conf) -> test(Conf, "dublincore", dublincore:wanted()).
itunes(Conf) -> test(Conf, "itunes", itunes:wanted()).

test_loop(_M, _F, _A, 0, List) ->
  List;
test_loop(M, F, A, N, List) ->
  {T, _Result} = timer:tc(M, F, A),
  test_loop(M, F, A, N - 1, [T|List]).

measure_time(M, F, A, N) when N > 0 ->
  L = test_loop(M, F, A, N, []),
  Length = length(L),
  Min = lists:min(L),
  Max = lists:max(L),
  Med = lists:nth(round((Length / 2)), lists:sort(L)),
  Avg = round(lists:foldl(fun(X, Sum) -> X + Sum end, 0, L) / Length),
  ct:log(info, ?LOW_IMPORTANCE, "Range: ~.2f - ~.2f ms~n"
    "Median: ~.2f ms~n"
    "Average: ~.2f ms~n",
    [X * 0.001 || X <- [Min, Max, Med, Avg]]),
  ok.

measure_time(Conf) ->
  Filename = name(Conf, "df"),
  measure_time(feeder, file, [Filename, []], 100).
