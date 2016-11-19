
-module(author).
-export([wanted/0]).

-include("../../src/feeder_records.hrl").

wanted() -> {
  #feed{
  },
  [
    #entry{
      authors = [#author{name= <<"John Gruber">>, url= <<"http://daringfireball.net/">>}],
      links = [#link{url = <<"http://df4.us/npk">>, rel= <<"shorturl">>},
              #link{url = <<"http://daringfireball.net/2015/03/apple_watch_prelude">>, rel= <<"alternate">>, type= <<"text/html">>}]
    }
  ]
  }.
