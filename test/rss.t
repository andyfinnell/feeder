#! /usr/bin/env escript

-include("../include/feeder.hrl").

main([]) ->
  code:add_pathz("test"),
  code:add_pathz("ebin"),
  etap:plan(8),
  file(),
  stream(),
  etap:end_tests().

file() ->
  Wanted = wanted(),
  Found = lists:reverse(common:file("test/rss.xml")),
  etap:is(4, length(Found), "should be 4"),
  common:is(Wanted, Found),
  ok.

wanted() -> 
  [#entry{
      title = <<"Star City">>,
      link = <<"http://liftoff.msfc.nasa.gov/news/2003/news-starcity.asp">>,
      summary= <<"How do Americans get ready to work with Russians aboard the International Space Station? They take a crash course in culture, language and protocol at Russia's <a href=\"http://howe.iki.rssi.ru/GCTC/gctc_e.htm\">Star City</a>.">>,
updated= <<"Tue, 03 Jun 2003 09:39:21 GMT">>,
id= <<"http://liftoff.msfc.nasa.gov/2003/06/03.html#item573">>
},
#entry{
  summary= <<"Sky watchers in Europe, Asia, and parts of Alaska and Canada will experience a <a href=\"http://science.nasa.gov/headlines/y2003/30may_solareclipse.htm\">partial eclipse of the Sun</a> on Saturday, May 31st.">>,
updated= <<"Fri, 30 May 2003 11:06:42 GMT">>,
id= <<"http://liftoff.msfc.nasa.gov/2003/05/30.html#item572">>
},
#entry{
  title = <<"The Engine That Does More">>,
  link = <<"http://liftoff.msfc.nasa.gov/news/2003/news-VASIMR.asp">>,
  summary= <<"Before man travels to Mars, NASA hopes to design new engines that will let us fly through the Solar System more quickly.  The proposed VASIMR engine would do that.">>,
  updated= <<"Tue, 27 May 2003 08:37:32 GMT">>,
  id= <<"http://liftoff.msfc.nasa.gov/2003/05/27.html#item571">>
},
#entry{
  title = <<"Astronauts' Dirty Laundry">>,
  link = <<"http://liftoff.msfc.nasa.gov/news/2003/news-laundry.asp">>,
  summary= <<"Compared to earlier spacecraft, the International Space Station has many luxuries, but laundry facilities are not one of them.  Instead, astronauts have other options.">>,
  updated= <<"Tue, 20 May 2003 08:56:02 GMT">>,
  id= <<"http://liftoff.msfc.nasa.gov/2003/05/20.html#item570">>
}
].

%% TODO: stream file
stream() ->
  Entries = common:stream("<channel><item><author>John Doe</author><title>Nice Title</title></item><item><title>Nice Title</title><author>John Doe</author></item></channel>"),
  Entry = #entry{author = <<"John Doe">>, title = <<"Nice Title">>},
  [etap:is(E, Entry,  "should be wanted") || E <- Entries],
  etap:is(2, length(Entries), "should be 2"),
  ok.