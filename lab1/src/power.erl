-module(power).

%% API
-export([power/2]).

power(A, B) ->
  if
    B == 0 -> 1;
    true -> A * power(A, B - 1)
  end.
