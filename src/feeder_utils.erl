-module(feeder_utils).

-export([not_undefined/2]).

%% Prefer the 1st value if it exists
not_undefined(undefined, Value2) ->
  Value2;
not_undefined(Value1, _Value2) ->
  Value1.
