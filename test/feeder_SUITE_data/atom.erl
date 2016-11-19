
-module(atom).
-export([wanted/0]).

-include("../../src/feeder_records.hrl").

wanted() -> {
  #feed{
    authors = [#author{name = <<"John Doe">>}],
    id = <<"urn:uuid:60a76c80-d399-11d9-b93C-0003939e0af6">>,
    links = [#link{url = <<"http://example.org/">>}],
    title = #text{value = <<"Example Feed">>},
    updated = <<"Sun, 18 May 2014 16:13:31 GMT">>
  },
  [
    #entry{
      id = <<"urn:uuid:1225c695-cfb8-4ebb-aaaa-80da344efa6a">>,
      links = [#link{url = <<"http://example.org/2003/12/13/atom03">>}],
      summary = #text{value = <<"Some text.">>},
      title = #text{value = <<"Atom-Powered Robots Run Amok">>},
      updated = <<"Sun, 18 May 2014 16:13:31 GMT">>    
    }
  ]
  }.
