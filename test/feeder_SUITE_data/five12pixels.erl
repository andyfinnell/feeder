
-module(five12pixels).
-export([wanted/0]).

-include("../../src/feeder_records.hrl").

wanted() -> {
  #feed{
    image = <<"https://static.feedpress.it/logo/512pixels.png">>,
    language = <<"en-US">>,
    links = [ 
              #link{url = <<"http://feedpress.superfeedr.com/">>, rel = <<"hub">>},
              #link{url = <<"https://feedpress.me/512pixels">>, rel = <<"self">>, type = <<"application/rss+xml">>},
              #link{url = <<"http://512pixels.net">>}
    ],
    title = #text{value = <<"512 Pixels">>},
    summary = #text{value = <<"512 Pixels is a blog written by Stephen Hackett about things that light up and make noise.">>},
    updated = <<"Sun, 02 Oct 2016 01:43:13 +0000">>,
    ttl = 60
  },      
  [
    #entry{
      authors = [#author{name = <<"Stephen">>}],
      id = <<"https://512pixels.net/?p=13155">>,
      links = [ #link{url = <<"https://512pixels.net/2016/10/drang-drafts-macs/">>} ],
      title = #text{value = <<"Drang Drafts Macs">>},
      summary = #text{value = <<"Dr. Drang has some excellent follow-up for the Mac draft I held earlier this week."/utf8>>},
      content = #text{value = <<"<p>Dr. Drang has <a href=\"http://leancrew.com/all-this/2016/09/four-or-more-macs/\">some excellent follow-up</a> for <a href=\"https://www.relay.fm/b-sides/26\">the Mac draft</a> I held earlier this week.</p>"/utf8>>},
      categories = [#category{term = <<"Apple">>}, #category{term = <<"Apple History">>}],
      updated = <<"Sat, 01 Oct 2016 17:16:20 +0000">> 
    },
        
    #entry{
      authors = [#author{name = <<"Stephen">>}],
      id = <<"https://512pixels.net/?p=13154">>,
      links = [ #link{url = <<"https://512pixels.net/2016/09/september-thank-you/">>} ],
      title = #text{value = <<"September: Thank You">>},
      summary = #text{value = <<"Instead of running RSS sponsorships this month, I’m raising money in support of St. Jude Children’s Research Hospital as part of Childhood Cancer Awareness Month. Click here to learn more and donate. Today is September 30, and the end of Childhood Cancer Awareness Month. I&#8217;d like to thank everyone who donated to support the work [&#8230;]"/utf8>>},
      content = #text{value = <<"<p><em>Instead of running RSS sponsorships this month, I’m raising money in support of St. Jude Children’s Research Hospital as part of Childhood Cancer Awareness Month. <a href=\"http://heroes.stjude.org/stephenhackett\">Click here to learn more and donate.</a></em></p>"/utf8>>},
      categories = [#category{term = <<"Sponsor">>}],
      updated = <<"Fri, 30 Sep 2016 15:13:57 +0000">> 
    }    
  ]
  }.
