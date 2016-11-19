
-module(globnerdishness).
-export([wanted/0]).

-include("../../src/feeder_records.hrl").

wanted() -> {
  #feed{
    authors = [#author{name = <<"Nathan Vander Wilt">>}],
    id = <<"http://www.extinguishedscholar.com/wpglob/?feed=atom">>,
    links = [ 
              #link{url = <<"http://n.exts.ch/?format=atom">>, rel = <<"self">>}
    ],
    title = #text{value = <<"a glob of nerdishness">>},
    updated = <<"2015-04-16T13:28:12-07:00">>
  },
  [
    #entry{
      id = <<"http://n.exts.ch/2015/04/f1040_spreadsheet">>,
      links = [ #link{url = <<"http://n.exts.ch/2015/04/f1040_spreadsheet">>, rel = <<"alternate">>} ],
      title = #text{value = <<"Numbers.app spreadsheet template for 2014 IRS Form 1040">>},
      content = #text{value = <<"<p><img alt=\"Sample table from spreadsheet, Schedule C expense categories\" src=\"http://n.exts.ch/resource/post-fcf4a38f1f9fe79c3a2b105f9d001454/Screen+Shot+2015-04-16+at+11.08.55+AM.png\"></img></p>"/utf8>>, type = <<"html">>},
      updated = <<"2015-04-16T13:28:12-07:00">> 
    },
                
    #entry{
      id = <<"http://n.exts.ch/2014/05/fat_chat">>,
      links = [ #link{url = <<"http://n.exts.ch/2014/05/fat_chat">>, rel = <<"alternate">>} ],
      title = #text{value = <<"FAT in a fortnight">>},
      content = #text{value = <<"<p>My <a href=\"https://github.com/natevw\">GitHub account</a> has been busy lately, thanks to not just one but two great open source–supporting clients!</p>"/utf8>>, type = <<"html">>},
      updated = <<"2014-05-30T10:20:16-07:00">> 
    }
  ]
  }.
