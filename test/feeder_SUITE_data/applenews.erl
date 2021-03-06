
-module(applenews).
-export([wanted/0]).

-include("../../src/feeder_records.hrl").

wanted() -> {
  #feed{
    language = <<"en-us">>,
    links = [#link{url = <<"http://www.apple.com/hotnews/">>}],
    title = #text{value = <<"Apple Hot News">>},
    summary = #text{value = <<"Hot News provided by Apple.">>},
    updated = <<"Tue, 26 Apr 2016 15:53:26 PDT">>,
    published = <<"Tue, 26 Apr 2016 15:53:26 PDT">>,
    categories = [#category{term = <<"Apple">>}]
  },  
  [
    #entry{
      links = [#link{url = <<"http://www.apple.com/pr/library/2016/04/26Apple-Reports-Second-Quarter-Results.html?sr=hotnews.rss">>}],
      summary = #text{value = <<"Apple today announced financial results for its fiscal 2016 second quarter ended March 26. The company posted quarterly revenue of $50.6 billion and quarterly net income of $10.5 billion, or $1.90 per diluted share. These results compare to revenue of $58 billion and net income of $13.6 billion, or $2.33 per diluted share, in the year-ago quarter. Gross margin was 39.4 percent compared to 40.8 percent in the year-ago quarter. International sales accounted for 67 percent of the quarter’s revenue. “Our team executed extremely well in the face of strong macroeconomic headwinds,” said Tim Cook, Apple’s CEO. “We are very happy with the continued strong growth in revenue from Services, thanks to the incredible strength of the Apple ecosystem and our growing base of over 1 billion active devices.”"/utf8>>},
      title = #text{value = <<"Apple Reports Second Quarter Results">>},
      published = <<"Tue, 26 Apr 2016 14:44:21 PDT">> 
    },
    #entry{
      links = [#link{url = <<"http://www.apple.com/final-cut-pro/in-action/trim-editing/?sr=hotnews.rss">>}],
      summary = #text{value = <<"When Trim Editing started creating music videos over a decade ago, just paying the rent was a huge accomplishment. Now, the small East London company is crafting award-winning visuals for big brands — like Audi, Nike, Adidas, and Guinness — propelled by the power of Final Cut Pro X. The video editing software’s comprehensive features allow Trim Editing to organize film and audio clips, pull together compelling projects, and make changes on the fly. “When I’m playing back an edit for a director, they’ll say, ‘Okay, let’s go and make those changes I talked about.’ I’ll say, ‘Oh, no, they’re already done,’ and we’ll jump back and watch it again. People can’t believe that I’ve magically done the change before we even finish playback,” says editor Thomas Grove Carter."/utf8>>},
      title = #text{value = <<"Final Cut Pro X helps small company delight world’s biggest clients"/utf8>>},
      published = <<"Wed, 20 Apr 2016 10:05:59 PDT">> 
    },
    #entry{
      links = [#link{url = <<"http://www.apple.com/ipad-pro/?sr=hotnews.rss">>}],
      summary = #text{value = <<"Apple today introduced the 9.7-inch iPad Pro, which at just under one pound features a new pro Retina display with greater brightness, wider color gamut, lower reflectivity, Night Shift mode, and new True Tone display technology. The new iPad Pro also has a 64-bit A9X chip that rivals most portable PCs. “iPad Pro is a new generation of iPad that is indispensable and immersive, enabling people to be more productive and more creative. It’s incredibly fast, extremely portable, and completely natural to use with your fingers, Apple Pencil, and Smart Keyboard. And now it comes in two sizes,” said Philip Schiller, Apple’s senior vice president of Worldwide Marketing."/utf8>>},
      title = #text{value = <<"Apple Introduces 9.7-inch iPad Pro"/utf8>>},
      published = <<"Mon, 21 Mar 2016 12:00:03 PDT">> 
    }    
  ]
  }.
