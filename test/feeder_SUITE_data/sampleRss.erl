
-module(sampleRss).
-export([wanted/0]).

-include("../../src/feeder_records.hrl").

wanted() -> {
  #feed{
    language = <<"en-us">>,
    image = <<"http://writetheweb.com/images/mynetscape88.gif">>,
    links = [ 
              #link{url = <<"http://writetheweb.com">>}
    ],
    title = #text{value = <<"WriteTheWeb">>},
    summary = #text{value = <<"News for web users that write back">>}
  },   
  [
    #entry{
      title = #text{value = <<"Giving the world a pluggable Gnutella">>},
      links = [#link{url = <<"http://writetheweb.com/read.php?item=24">>}],
      summary = #text{value = <<"WorldOS is a framework on which to build programs that work like Freenet or Gnutella -allowing distributed applications using peer-to-peer routing."/utf8>>}
    },    
    #entry {
      title = #text{value = <<"Syndication discussions hot up">>},
      links = [ #link{url = <<"http://writetheweb.com/read.php?item=23">>}],
      summary = #text{value = <<"After a period of dormancy, the Syndication mailing list has become active again, with contributions from leaders in traditional media and Web syndication.">>}
    }
  ]
  }.
