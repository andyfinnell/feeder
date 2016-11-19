
-module(rss).
-export([wanted/0]).

-include("../../src/feeder_records.hrl").

wanted() -> {
  #feed{
    language = <<"en-us">>,
    links = [
      #link{rel= <<"via">>, url= <<"http://liftoff.msfc.nasa.gov/">>},
      #link{rel= <<"self">>, url= <<"http://liftoff.msfc.nasa.gov/">>, type= <<"application/rss+xml">>},
      #link{url= <<"http://liftoff.msfc.nasa.gov/">>}
    ],
    title = #text{value= <<"Liftoff News">>},
    summary = #text{value= <<"Liftoff to Space Exploration.">>},
    updated = <<"Tue, 10 Jun 2003 04:00:00 GMT">>
  },
  [
    #entry{
      title = #text{value= <<"Star City">>},
      links = [#link{url= <<"http://liftoff.msfc.nasa.gov/news/2003/news-starcity.asp">>},
              #link{url= <<"http://liftoff.msfc.nasa.gov/2003/06/03.html#item573">>}],
      summary = #text{value= <<"How do Americans get ready to work with Russians aboard the International Space Station? They take a crash course in culture, language and protocol at Russia's <a href=\"http://howe.iki.rssi.ru/GCTC/gctc_e.htm\">Star City</a>.">>},
      id = <<"http://liftoff.msfc.nasa.gov/2003/06/03.html#item573">>,
      updated = <<"Tue, 03 Jun 2003 09:39:21 GMT">>    
    },
    
    #entry{
      summary = #text{value= <<"Sky watchers in Europe, Asia, and parts of Alaska and Canada will experience a <a href=\"http://science.nasa.gov/headlines/y2003/30may_solareclipse.htm\">partial eclipse of the Sun</a> on Saturday, May 31st.">>},
      id = <<"http://liftoff.msfc.nasa.gov/2003/05/30.html#item572">>,
      links = [#link{url = <<"http://liftoff.msfc.nasa.gov/2003/05/30.html#item572">>}],
      updated = <<"Fri, 30 May 2003 11:06:42 GMT">>
    },
    
    #entry{
      title = #text{value= <<"The Engine That Does More">>},
      links = [#link{url= <<"http://liftoff.msfc.nasa.gov/news/2003/news-VASIMR.asp">>},
              #link{url= <<"http://liftoff.msfc.nasa.gov/2003/05/27.html#item571">>}],
      summary = #text{value= <<"Before man travels to Mars, NASA hopes to design new engines that will let us fly through the Solar System more quickly.  The proposed VASIMR engine would do that.">>},
      id = <<"http://liftoff.msfc.nasa.gov/2003/05/27.html#item571">>,
      updated = <<"Tue, 27 May 2003 08:37:32 GMT">>
    },
    
    #entry{
      title = #text{value= <<"Astronauts' Dirty Laundry">>},
      links = [#link{url= <<"http://liftoff.msfc.nasa.gov/news/2003/news-laundry.asp">>},
              #link{url = <<"http://liftoff.msfc.nasa.gov/2003/05/20.html#item570">>}],
      summary = #text{value= <<"Compared to earlier spacecraft, the International Space Station has many luxuries, but laundry facilities are not one of them.  Instead, astronauts have other options.">>},
      id = <<"http://liftoff.msfc.nasa.gov/2003/05/20.html#item570">>,
      updated = <<"Tue, 20 May 2003 08:56:02 GMT">>
    }
  ]
}.
