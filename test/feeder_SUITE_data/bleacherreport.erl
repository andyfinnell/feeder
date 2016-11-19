
-module(bleacherreport).
-export([wanted/0]).

-include("../../src/feeder_records.hrl").

wanted() -> {
  #feed{
    language = <<"en-us">>,
    links = [#link{url = <<"http://bleacherreport.com/">>}],
    title = #text{value = <<"Bleacher Report - MLS">>},
    summary = #text{value = <<"Bleacher Report - The open source sports network">>},
    ttl = 30
  },  
  [
    #entry{
      id = <<"http://bleacherreport.com/articles/2669541-resurgent-seattle-sounders-benefiting-from-new-mentality-under-interim-boss">>,
      authors = [#author{name = <<"Joe Tansey">>}],
      links = [#link{url = <<"http://bleacherreport.com/articles/2669541-resurgent-seattle-sounders-benefiting-from-new-mentality-under-interim-boss">>}],
      summary = #text{value = <<"<p class=\"ui-droppable\"><img class=\"slot\" src=\"/images/pixel.gif\" alt=\"\">Rock bottom for the Seattle Sounders came at the end of July.&nbsp;</p><p>Two days after their miserable performance against <a href=\"http://bleacherreport.com/sporting-kansas-city\">Sporting Kansas City</a> on July 24 resulted in a 3-0 loss, the Sounders parted ways with longtime boss Sigi Schmid, who led the club to four U.S. Open Cups and the 2014 Supporters' Shield.&nbsp;</p>"/utf8>>},
      title = #text{value = <<"Resurgent Seattle Sounders Benefiting from New Mentality Under Interim Boss">>},
      categories = [#category{term = <<"Soccer">>}, #category{term = <<"World Football">>}, #category{term = <<"MLS">>},
                    #category{term = <<"Opinion">>}, #category{term = <<"Seattle Sounders FC">>}, #category{term = <<"Artur Davtyan">>},
                    #category{term = <<"Serge Deblé"/utf8>>}],
      updated = <<"Fri, 14 Oct 2016 04:01:42 -0400">> 
    },
    #entry{
      id = <<"http://bleacherreport.com/articles/2669286-colorado-rapids-on-accelerated-path-toward-winning-trophies-in-2016">>,
      authors = [#author{name = <<"Joe Tansey">>}],
      links = [#link{url = <<"http://bleacherreport.com/articles/2669286-colorado-rapids-on-accelerated-path-toward-winning-trophies-in-2016">>}],
      summary = #text{value = <<"<p class=\"ui-droppable\"><img class=\"slot\" src=\"/images/pixel.gif\" alt=\"\">The path to achieve a string of advantageous goals was set out during the preseason by the <a href=\"http://bleacherreport.com/colorado-rapids\">Colorado Rapids</a>.&nbsp;They just didn't think they'd travel down that path so fast in only&nbsp;one season.&nbsp;</p><p>After finishing with the worst record in Major League Soccer's Western Conference a year ago, the Rapids have the inside track to win the Supporters' Shield, which is awarded to the team with the most regular-season points.&nbsp;</p>"/utf8>>},
      title = #text{value = <<"Colorado Rapids on Accelerated Path Toward Winning Trophies in 2016">>},
      categories = [#category{term = <<"Soccer">>}, #category{term = <<"World Football">>}, #category{term = <<"MLS">>},
                    #category{term = <<"Colorado Rapids">>}, #category{term = <<"Opinion">>}, #category{term = <<"Artur Davtyan">>},
                    #category{term = <<"Serge Deblé"/utf8>>}],
      updated = <<"Thu, 13 Oct 2016 10:43:52 -0400">> 
    }
  ]
  }.
