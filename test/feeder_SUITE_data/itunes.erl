
-module(itunes).
-export([wanted/0]).

-include("../../src/feeder_records.hrl").

wanted() -> {
  #feed{
    authors = [#author{name = <<"John Doe">>}],
    image = <<"http://example.com/podcasts/everything/AllAboutEverything.jpg">>,
    language = <<"en-us">>,
    links = [#link{url = <<"http://www.example.com/podcasts/everything/index.html">>}],
    subtitle = #text{value = <<"A show about everything">>},
    summary = #text{value = <<"All About Everything is a show about everything. Each week we dive into any subject known to man and talk about it as much as we can. Look for our Podcast in the iTunes Store">>},
    title = #text{value = <<"All About Everything">>},
    categories = [#category{term = <<"Gadgets">>}, #category{term = <<"Technology">>}, #category{term = <<"TV & Film">>}]
  },
  [
    #entry{
      authors = [#author{name= <<"John Doe">>}],
      duration = <<"7:04">>,
      links = [#link{rel = <<"enclosure">>, url= <<"http://example.com/podcasts/everything/AllAboutEverythingEpisode3.m4a">>, length = 8727310, type= <<"audio/x-m4a">>},
              #link{url = <<"http://example.com/podcasts/archive/aae20050615.m4a">>}],
      image = <<"http://example.com/podcasts/everything/AllAboutEverything/Episode1.jpg">>,
      id = <<"http://example.com/podcasts/archive/aae20050615.m4a">>,
      subtitle = #text{value= <<"Rückblicke, Einblicke und Ausblicke auf das netzpolitische Geschehen als wöchentlicher Podcast"/utf8>>},
      summary = #text{value= <<"This week we talk about salt and pepper shakers, comparing and contrasting pour rates, construction materials, and overall aesthetics. Come and join the party!">>},
      title = #text{value= <<"Shake Shake Shake Your Spices">>},
      published = <<"Wed, 15 Jun 2005 19:00:00 GMT">>
    },
    
    #entry{
      authors = [#author{name= <<"Jane Doe">>}],
      duration = <<"4:34">>,
      links = [#link{rel = <<"enclosure">>, url= <<"http://example.com/podcasts/everything/AllAboutEverythingEpisode2.mp3">>, length = 5650889, type= <<"audio/mpeg">>},
              #link{url = <<"http://example.com/podcasts/archive/aae20050608.mp3">>}],
      image = <<"http://example.com/podcasts/everything/AllAboutEverything/Episode2.jpg">>,
      id = <<"http://example.com/podcasts/archive/aae20050608.mp3">>,
      subtitle = #text{value= <<"Comparing socket wrenches is fun!">>},
      summary = #text{value= <<"This week we talk about metric vs. old english socket wrenches. Which one is better? Do you really need both? Get all of your answers here.">>},
      title = #text{value= <<"Socket Wrench Shootout">>},
      published = <<"Wed, 8 Jun 2005 19:00:00 GMT">>
    },
    
    #entry{
      authors = [#author{name= <<"Various">>}],
      duration = <<"3:59">>,
      links = [#link{rel = <<"enclosure">>, url= <<"http://example.com/podcasts/everything/AllAboutEverythingEpisode1.mp3">>, length = 4989537, type= <<"audio/mpeg">>},
              #link{url = <<"http://example.com/podcasts/archive/aae20050601.mp3">>}],
      image = <<"http://example.com/podcasts/everything/AllAboutEverything/Episode3.jpg">>,
      id = <<"http://example.com/podcasts/archive/aae20050601.mp3">>,
      subtitle = #text{value= <<"Red + Blue != Purple">>},
      summary = #text{value= <<"This week we talk about surviving in a Red state if you are a Blue person. Or vice versa.">>},
      title = #text{value= <<"Red, Whine, & Blue">>},
      published = <<"Wed, 1 Jun 2005 19:00:00 GMT">>
    }
  ]
}.
