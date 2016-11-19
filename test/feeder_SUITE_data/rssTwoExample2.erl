
-module(rssTwoExample2).
-export([wanted/0]).

-include("../../src/feeder_records.hrl").

wanted() -> {
  #feed{
    language = <<"en-us">>,
    links = [ 
              #link{url = <<"http://www.scripting.com/">>}
    ],
    title = #text{value = <<"Scripting News">>},
    summary = #text{value = <<"A weblog about scripting and stuff like that.">>},
    updated = <<"Mon, 30 Sep 2002 11:00:00 GMT">>,
    categories = [#category{term = <<"1765">>}],
    ttl = 40
  },    
  [
    #entry{
      id = <<"http://scriptingnews.userland.com/backissues/2002/09/29#When:6:56:02PM">>,
      links = [#link{url = <<"http://scriptingnews.userland.com/backissues/2002/09/29#When:6:56:02PM">>}],
      summary = #text{value = <<"\"rssflowersalignright\"With any luck we should have one or two more days of namespaces stuff here on Scripting News. It feels like it's winding down. Later in the week I'm going to a <a href=\"http://harvardbusinessonline.hbsp.harvard.edu/b02/en/conferences/conf_detail.jhtml?id=s775stg&pid=144XCF\">conference</a> put on by the Harvard Business School. So that should change the topic a bit. The following week I'm off to Colorado for the <a href=\"http://www.digitalidworld.com/conference/2002/index.php\">Digital ID World</a> conference. We had to go through namespaces, and it turns out that weblogs are a great way to work around mail lists that are clogged with <a href=\"http://www.userland.com/whatIsStopEnergy\">stop energy</a>. I think we solved the problem, have reached a consensus, and will be ready to move forward shortly."/utf8>>},
      updated = <<"Mon, 30 Sep 2002 01:56:02 GMT">>
    },
    #entry {
      id = <<"http://scriptingnews.userland.com/backissues/2002/09/29#lawAndOrder">>,
      title = #text{value = <<"Law and Order">>},
      links = [ #link{url = <<"http://scriptingnews.userland.com/backissues/2002/09/29#lawAndOrder">>}],
      summary = #text{value = <<"<p><a href=\"http://www.nbc.com/Law_&_Order/index.html\"><img src=\"http://radio.weblogs.com/0001015/images/2002/09/29/lenny.gif\" width=\"45\" height=\"53\" border=\"0\" align=\"right\" hspace=\"15\" vspace=\"5\" alt=\"A picture named lenny.gif\"></a>A great line in a recent Law and Order. Lenny Briscoe, played by Jerry Orbach, is interrogating a suspect. The suspect tells a story and reaches a point where no one believes him, not even the suspect himself. Lenny says: \"Now there's five minutes of my life that's lost forever.\" </p>">>},
      updated = <<"Sun, 29 Sep 2002 23:48:33 GMT">>
    }
  ]
  }.
