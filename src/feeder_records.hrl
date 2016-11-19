
-record(link, {
  url :: undefined | binary(),
  length :: undefined | binary(),
  type :: undefined | binary(),
  rel :: undefined | binary(),
  title :: undefined | binary()
}).

-record(author, {
  name :: undefined | binary(),
  url :: undefined | binary(),
  email :: undefined | binary()
}).

-record(text, {
  type :: undefined | binary(),
  value :: undefined | binary(),
  language :: undefined | binary()
}).

-record(category, {
  term :: undefined | binary(),
  scheme :: undefined | binary(),
  label :: undefined | binary()
}).

-record(feed, {
  authors :: undefined | [author()],
  id :: undefined | binary(),
  image :: undefined | binary(),
  language :: undefined | binary(),
  links :: undefined | [link()],
  published :: undefined | binary(),
  subtitle :: undefined | text(),
  summary :: undefined | text(),
  title :: undefined | text(),
  updated :: undefined | binary(),
  categories :: undefined | [category()],
  ttl :: undefined | pos_integer()
}).

-record(entry, {
  authors :: undefined | [author()],
  duration :: undefined | binary(),
  links :: undefined | [link()],
  id :: undefined | binary(),
  image :: undefined | binary(),
  published :: undefined | binary(),
  subtitle :: undefined | text(),
  summary :: undefined | text(),
  title :: undefined | text(),
  updated :: undefined | binary(),
  content :: undefined | text(),
  categories :: undefined | [category()]
}).

-type author() :: #author{}.
-type entry() :: #entry{}.
-type link() :: #link{}.
-type feed() :: #feed{}.
-type text() :: #text{}.
-type category() :: #category{}.