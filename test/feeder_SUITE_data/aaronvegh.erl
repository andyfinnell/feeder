
-module(aaronvegh).
-export([wanted/0]).

-include("../../src/feeder_records.hrl").

wanted() -> {
  #feed{
    links = [#link{url = <<"http://aaron.vegh.ca/">>}, 
            #link{url = <<"http://aaron.vegh.ca/rss/">>, rel= <<"self">>, type= <<"application/rss+xml">>}],
    title = #text{value = <<"Aaron Vegh">>},
    summary = #text{value = <<"Adventures in Cooca">>},
    updated = <<"Sun, 04 Sep 2016 00:15:10 GMT">>,
    ttl = 60
  },
  [
    #entry{
      id = <<"52572999-4962-4503-9fe0-d9e121b76dfe">>,
      authors = [#author{name = <<"Aaron Vegh">>}],
      links = [#link{url = <<"http://aaron.vegh.ca/2016/08/failing-early-failing-often/">>}],
      summary = #text{value = <<"<p>It's Sunday morning and I'm reflecting on my failures.</p>">>},
      content = #text{value = <<"<p>It's Sunday morning and I'm reflecting on my failures.</p>">>},
      title = #text{value = <<"Failing Often">>},
      published = <<"Sun, 14 Aug 2016 16:11:20 GMT">> 
    },
    #entry{
      id = <<"5e0c77e0-7d9c-405e-bec4-163d5e69d66f">>,
      authors = [#author{name = <<"Aaron Vegh">>}],
      links = [#link{url = <<"http://aaron.vegh.ca/2016/06/the-dojo-in-your-mind/">>}],
      summary = #text{value = <<"<p>I just read Brian Gilham's piece, <a href=\"https://briangilham.com/you-dont-need-a-computer-science-degree-aa1962a9e2b5#.wd7u9o5id\">You Don't Need a Computer Science Degree</a>, which simply makes the point that prospective iOS developers (or let's face it, <em>any developer</em>) don't need a computer science degree to become successful in this field.</p>">>},
      content = #text{value = <<"<p>I just read Brian Gilham's piece, <a href=\"https://briangilham.com/you-dont-need-a-computer-science-degree-aa1962a9e2b5#.wd7u9o5id\">You Don't Need a Computer Science Degree</a>, which simply makes the point that prospective iOS developers (or let's face it, <em>any developer</em>) don't need a computer science degree to become successful in this field.</p>">>},
      title = #text{value = <<"The Dojo in Your Mind">>},
      published = <<"Tue, 07 Jun 2016 13:20:59 GMT">> 
    },
    #entry{
      id = <<"c6d425bf-a8d5-4497-a989-0a46a270b180">>,
      authors = [#author{name = <<"Aaron Vegh">>}],
      links = [#link{url = <<"http://aaron.vegh.ca/2016/05/microelectronics-are-my-new-jam/">>}],
      summary = #text{value = <<"<p><img src=\"http://aaron.vegh.ca/content/images/2016/05/IMG_3343-2.jpg\" alt=\"\"> I've spent most of the past decade with a near-maniacal, laser-guided focus on one ambition: to become an indie software developer. It officially kicked off after seeing Daniel Jalkut give a talk at <a href=\"https://en.wikipedia.org/wiki/C4_(conference)\">C4</a> in 2007. Success hasn't turned out to be exactly what I thought, though it would be</p>">>},
      content = #text{value = <<"<p><img src=\"http://aaron.vegh.ca/content/images/2016/05/IMG_3343-2.jpg\" alt=\"\"> I've spent most of the past decade with a near-maniacal, laser-guided focus on one ambition: to become an indie software developer. </p>">>},
      title = #text{value = <<"Microelectronics are my new jam.">>},
      published = <<"Fri, 06 May 2016 17:20:24 GMT">> 
    },    
    #entry{
      id = <<"176c8e1e-4b2d-449e-9d5f-6cfb262416f2">>,
      authors = [#author{name = <<"Aaron Vegh">>}],
      links = [#link{url = <<"http://aaron.vegh.ca/2016/02/new-blog-new-something-something/">>}],
      summary = #text{value = <<"<p>I've been writing on this blog since 2008, and in that time it's been all Wordpress. In these last eight years I've seen Wordpress rise, become enormously popular, turn into a CMS and a replacement for web development altogether. I've also seen it turn into a popular vector for hackers.</p>">>},
      content = #text{value = <<"<img src=\"http://aaron.vegh.ca/content/images/2016/02/image-1.jpeg\" alt=\"New Blog, New Something Something\"><p>I've been writing on this blog since 2008, and in that time it's been all Wordpress. </p>">>},
      title = #text{value = <<"New Blog, New Something Something">>},
      published = <<"Mon, 15 Feb 2016 01:45:24 GMT">> 
      %%  <media:content url="http://aaron.vegh.ca/content/images/2016/02/image-1.jpeg" medium="image"/>
    },    
    #entry{
      id = <<"32d8ef8a-2b1d-48d4-9adb-24db0f4ed5ee">>,
      authors = [#author{name = <<"Aaron Vegh">>}],
      links = [#link{url = <<"http://aaron.vegh.ca/2015/11/why-contracting-is-the-answer/">>}],
      summary = #text{value = <<"<p>I got my first job out of school back in 1998. I was responsible for building and maintaining a website for a Hamilton-based magazine publisher. Less than three months into it, the publisher fired me: we appeared to have fundamental differences of opinion on what my job actually was. I</p>">>},
      content = #text{value = <<"<p>I got my first job out of school back in 1998.</p>">>},
      title = #text{value = <<"Why Contracting is the Answer">>},
      published = <<"Tue, 03 Nov 2015 14:26:53 GMT">> 
    },    
    #entry{
      id = <<"7f3bde29-483c-432b-bb18-a772eccce08c">>,
      authors = [#author{name = <<"Aaron Vegh">>}],
      links = [#link{url = <<"http://aaron.vegh.ca/2015/10/appletv-and-the-icloudpocalypse/">>}],
      summary = #text{value = <<"<p>First, the good news: Magpie for AppleTV is coming.</p>">>},
      content = #text{value = <<"<p>First, the good news: Magpie for AppleTV is coming. Here’s proof!</p>"/utf8>>},
      title = #text{value = <<"AppleTV and the iCloudpocalypse">>},
      published = <<"Fri, 23 Oct 2015 13:58:50 GMT">> 
    },
    #entry{
      id = <<"51987301-ff62-483c-ae24-380dd5ac1211">>,
      authors = [#author{name = <<"Aaron Vegh">>}],
      links = [#link{url = <<"http://aaron.vegh.ca/2015/10/disingenuous/">>}],
      summary = #text{value = <<"<p>This is a post about Marco Arment’s decision to implement patronage pricing for his app, Overcast.</p>"/utf8>>},
      content = #text{value = <<"<p>This is a post about Marco Arment’s decision to implement patronage pricing for his app, Overcast. </p>"/utf8>>},
      title = #text{value = <<"Disingenuous">>},
      published = <<"Thu, 22 Oct 2015 00:04:52 GMT">> 
    },    
    #entry{
      id = <<"0fc7f46a-f458-4d68-afd4-2818e0f1ac34">>,
      authors = [#author{name = <<"Aaron Vegh">>}],
      links = [#link{url = <<"http://aaron.vegh.ca/2015/08/5000-years-later/">>}],
      summary = #text{value = <<"<p><img src=\"http://aaron.vegh.ca/content/images/2015/08/Seveneves.png\" alt=\"Seveneves\" /></p><p><strong>Warning: Spoilers</strong></p>">>},
      content = #text{value = <<"<p><img src=\"http://aaron.vegh.ca/content/images/2015/08/Seveneves.png\" alt=\"Seveneves\" /></p><p><strong>Warning: Spoilers</strong></p>">>},
      title = #text{value = <<"5000 Years Later...">>},
      published = <<"Fri, 21 Aug 2015 11:36:10 GMT">> 
    },                
    #entry{
      id = <<"d9594a31-fba7-44f9-a8bc-1d9a67db819a">>,
      authors = [#author{name = <<"Aaron Vegh">>}],
      links = [#link{url = <<"http://aaron.vegh.ca/2015/08/im-looking-for-a-partner/">>}],
      summary = #text{value = <<"<p>Here’s the short version: I’m looking for someone to team up with, hopefully for the long term.</p>"/utf8>>},
      content = #text{value = <<"<p>Here’s the short version: I’m looking for someone to team up with, hopefully for the long term. Someone who can take what I build, and make sure the right people know about it.</p>"/utf8>>},
      title = #text{value = <<"I'm Looking for a Partner">>},
      published = <<"Sun, 16 Aug 2015 16:12:51 GMT">> 
    },
    #entry{
      id = <<"c21ed0cb-cfc9-4a4a-a87c-d15ed9c35c4d">>,
      authors = [#author{name = <<"Aaron Vegh">>}],
      links = [#link{url = <<"http://aaron.vegh.ca/2014/12/apples-indifference-to-developers/">>}],
      summary = #text{value = <<"<p>Developer Greg Gardner, whose app was rejected in September for violated unpublished guidelines, <a href=\"https://cromulentlabs.wordpress.com/2014/12/07/launcher-followup-and-thoughts-on-the-app-store-review-system/\">wrote a detailed account of the process</a>.</p>">>},
      content = #text{value = <<"<p>Developer Greg Gardner, whose app was rejected in September for violated unpublished guidelines, <a href=\"https://cromulentlabs.wordpress.com/2014/12/07/launcher-followup-and-thoughts-on-the-app-store-review-system/\">wrote a detailed account of the process</a>.</p>">>},
      title = #text{value = <<"Apple's Indifference to Developers">>},
      published = <<"Tue, 09 Dec 2014 11:59:21 GMT">> 
    }    
  ]
  }.
