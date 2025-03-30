-module(qsort).
-import(timer, [tc/2]).
-import(lists, [sort/1]).

%% API
-export([qs/1, random_elems/3, compare_speeds/3]).

less_than(List, Arg) -> [X || X <- List, X < Arg].

grt_eq_than(List, Arg) -> [X || X <- List, X >= Arg].

qs([]) -> [];
qs([Pivot | Tail]) ->
  qs( less_than(Tail, Pivot) ) ++ [Pivot] ++ qs( grt_eq_than(Tail, Pivot) ).

random_elems(N, Min, Max) -> [rand:uniform(Max) + Min || _ <- lists:seq(1, N)].

compare_speeds(List, Fun1, Fun2) ->
  {Time1, _} = timer:tc(Fun1, [List]),
  io:format("Czas pierwszy: ~w~n", [Time1]),
  {Time2, _} = timer:tc(Fun2, [List]),
  io:format("Czar drugi: ~w~n", [Time2]).