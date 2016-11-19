
-module(gratefulDead).
-export([wanted/0]).

-include("../../src/feeder_records.hrl").

wanted() -> {
  #feed{
    links = [ 
              #link{url = <<"http://www.scripting.com/blog/categories/gratefulDead.html">>}
    ],
    title = #text{value = <<"Dave Winer: Grateful Dead">>},
    summary = #text{value = <<"A high-fidelity Grateful Dead song every day. This is where we're experimenting with enclosures on RSS news items that download when you're not using your computer. If it works (it will) it will be the end of the Click-And-Wait multimedia experience on the Internet.">>},
    updated = <<"Fri, 13 Apr 2001 19:23:02 GMT">>
  },  
  [
    #entry{
      links = [ #link{url = <<"http://www.scripting.com/mp3s/weatherReportDicksPicsVol7.mp3">>, rel = <<"enclosure">>, length = <<"6182912">>, type = <<"audio/mpeg">>} ],
      summary = #text{value = <<"It's been a few days since I added a song to the Grateful Dead channel. Now that there are all these new Radio users, many of whom are tuned into this channel (it's #16 on the hotlist of upstreaming Radio users, there's no way of knowing how many non-upstreaming users are subscribing, have to do something about this..). Anyway, tonight's song is a live version of Weather Report Suite from Dick's Picks Volume 7. It's wistful music. Of course a beautiful song, oft-quoted here on Scripting News. <i>A little change, the wind and rain.</i>"/utf8>>}
    },
    #entry {
      summary = #text{value = <<"Kevin Drennan started a <a href=\"http://deadend.editthispage.com/\">Grateful Dead Weblog</a>. Hey it's cool, he even has a <a href=\"http://deadend.editthispage.com/directory/61\">directory</a>. <i>A Frontier 7 feature.</i>">>}
    },
    #entry{
      links = [ #link{url = <<"http://www.scripting.com/mp3s/theOtherOne.mp3">>, rel = <<"enclosure">>, length = <<"6666097">>, type = <<"audio/mpeg">>} ],
      summary = #text{value = <<"<a href=\"http://arts.ucsc.edu/GDead/AGDL/other1.html\">The Other One</a>, live instrumental, One From The Vault. Very rhythmic very spacy, you can listen to it many times, and enjoy something new every time."/utf8>>}
    },
    #entry {
      summary = #text{value = <<"This is a test of a change I just made. Still diggin..">>}
    }
  ]
  }.
