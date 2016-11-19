
-module(cabel).
-export([wanted/0]).

-include("../../src/feeder_records.hrl").

wanted() -> {
  #feed{
    id = <<"tag:blogger.com,1999:blog-14853865">>,
    authors = [#author{name = <<"Cabel">>, url = <<"http://www.blogger.com/profile/01535868020374485339">>, email = <<"noreply@blogger.com">>}],
    links = [ #link{url = <<"http://www.blogger.com/feeds/14853865/posts/default">>, rel = <<"self">>, type = <<"application/atom+xml">>},
              #link{url = <<"http://www.cabel.name/">>, rel = <<"alternate">>, type = <<"text/html">>},    
              #link{url = <<"http://pubsubhubbub.appspot.com/">>, rel = <<"hub">>},    
              #link{url = <<"http://www.blogger.com/feeds/14853865/posts/default?start-index=26&max-results=25">>, rel = <<"next">>, type = <<"application/atom+xml">>},    
              #link{url = <<"http://cabel.name/atom.xml">>, rel = <<"http://schemas.google.com/g/2005#feed">>, type = <<"application/atom+xml">>}
    ],
    title = #text{value = <<"cabel.name">>, type = <<"text">>},
    subtitle = #text{value = <<"Cabel Sasser's Internet Blog. Hooray!">>, type = <<"html">>},
    updated = <<"2010-05-13T08:41:32.033-07:00">>
  },    
  [
    #entry{
      id = <<"tag:blogger.com,1999:blog-14853865.post-2537854613260204594">>,
      authors = [#author{name = <<"Cabel">>, url = <<"http://www.blogger.com/profile/01535868020374485339">>, email = <<"noreply@blogger.com">>}],
      links = [ #link{url = <<"http://www.blogger.com/feeds/14853865/2537854613260204594/comments/default">>, rel = <<"replies">>, type = <<"application/atom+xml">>, title = <<"Post Comments">>},
                #link{rel = <<"replies">>, type = <<"text/html">>, url = <<"http://www.cabel.name/2009/10/best-fast-food-receipt.html#comment-form">>, title = <<"41 Comments">>},
                #link{rel = <<"edit">>, type = <<"application/atom+xml">>, url = <<"http://www.blogger.com/feeds/14853865/posts/default/2537854613260204594">>},
                #link{rel = <<"self">>, type = <<"application/atom+xml">>, url = <<"http://www.blogger.com/feeds/14853865/posts/default/2537854613260204594">>},
                #link{rel = <<"alternate">>, type = <<"text/html">>, url = <<"http://www.cabel.name/2009/10/best-fast-food-receipt.html">>, title = <<"The Best Fast Food Receipt">>}
      ],
      title = #text{value = <<"The Best Fast Food Receipt">>, type= <<"text">>},
      content = #text{value = <<"<div class=\"photoblock-left\"><img src=\"http://www.cabel.name/images-post/2009/10/receipt-350.png\" class=\"photoborder\"></div>I'm not a real \"fast food\" guy. I don't really love burgers, and I don't really love super fried or greasy things, and I realize that by making both of these statements it's very possible that the Department of Bro-land Security will revoke my man-certificate.<br />">>, type = <<"html">>},
      updated = <<"2009-10-12T13:39:32.014-07:00">> 
    },
    
    #entry{
      id = <<"tag:blogger.com,1999:blog-14853865.post-3886385625844000508">>,
      updated = <<"2009-09-30T14:54:27.737-07:00">>,
      title = #text{type = <<"text">>, value = <<"Kashiwa Mystery Cafe">>},
      content = #text{type = <<"html">>, value = <<"So, we're in Japan, and we've just stepped off the train in Kashiwa, a very nice town in Chiba which also happens to be the home of Panic's Japanese HQ, which itself happens to be essentially a cozy apartment, which reminds me of Panic USA 1.0, except Noby and Kenichi don't <i>live</i> in the apartment, and the apartment is in Japan.<br />">>},
      links = [
        #link{rel = <<"replies">>, type = <<"application/atom+xml">>, url = <<"http://www.blogger.com/feeds/14853865/3886385625844000508/comments/default">>, title = <<"Post Comments">>},
        #link{rel = <<"replies">>, type = <<"text/html">>, url = <<"http://www.cabel.name/2009/09/kashiwa-mystery-cafe.html#comment-form">>, title = <<"100 Comments">>},
        #link{rel = <<"edit">>, type = <<"application/atom+xml">>, url = <<"http://www.blogger.com/feeds/14853865/posts/default/3886385625844000508">>},
        #link{rel = <<"self">>, type = <<"application/atom+xml">>, url = <<"http://www.blogger.com/feeds/14853865/posts/default/3886385625844000508">>},
        #link{rel = <<"alternate">>, type = <<"text/html">>, url = <<"http://www.cabel.name/2009/09/kashiwa-mystery-cafe.html">>, title = <<"Kashiwa Mystery Cafe">>}        
      ],
      authors = [#author{name = <<"Cabel">>, url = <<"http://www.blogger.com/profile/01535868020374485339">>, email = <<"noreply@blogger.com">>}]
    }
    
  ]
  }.
