
-module(cnn).
-export([wanted/0]).

-include("../../src/feeder_records.hrl").

wanted() -> {
  #feed{
    image = <<"http://i2.cdn.turner.com/cnn/2015/images/09/24/cnn.digital.png">>,
    language = <<"en-US">>,
    links = [ #link{url = <<"http://www.cnn.com/mobile-app-manual/index.html">>},
              #link{url = <<"http://rss.cnn.com/rss/cnn_topstories">>, rel = <<"self">>, type = <<"application/rss+xml">>},    
              #link{url = <<"http://pubsubhubbub.appspot.com/">>, rel = <<"hub">>}
    ],
    title = #text{value = <<"CNN.com - RSS Channel - Mobile App Manual">>},
    summary = #text{value = <<"CNN.com delivers up-to-the-minute news and information on the latest top stories, weather, entertainment, politics and more.">>},
    updated = <<"Sat, 15 Oct 2016 20:08:03 GMT">>,
    ttl = 10
  },    
  [
    #entry{
      id = <<"http://www.cnn.com/2016/10/14/us/mosque-attack-thwarted-kansas/index.html">>,
      links = [ #link{url = <<"http://rss.cnn.com/~r/rss/cnn_topstories/~3/pfH2e3yEaoc/index.html">>} ],
      title = #text{value = <<"Militia plot to bomb Somalis in KS thwarted, feds say">>},
      summary = #text{value = <<"Three men have been charged with plotting to bomb a mosque and apartment complex occupied by Somali immigrants in Kansas, federal officials say.">>},
      updated = <<"Sat, 15 Oct 2016 00:49:19 GMT">> 
    },
    #entry{
      id = <<"http://www.cnn.com/2016/10/15/politics/donald-trump-hillary-clinton-drug-test/index.html">>,
      links = [ #link{url = <<"http://rss.cnn.com/~r/rss/cnn_topstories/~3/45DZCmwONUs/index.html">>} ],
      title = #text{value = <<"Clinton got 'pumped up' for debate, Trump claims">>},
      summary = #text{value = <<"Donald Trump suggested Saturday that his Democratic rival, Hillary Clinton, has been \"getting pumped up\" with performance-enhancing drugs and challenged Clinton to take a drug test before the final debate next week.">>},
      updated = <<"Sat, 15 Oct 2016 17:58:34 GMT">> 
    }
  ]
  }.
