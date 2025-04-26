-module(myLists).

%% API
-export([contains/2, duplicateElements/1, sumFloats/1, sumFloatsTail/1]).

contains([], _) -> false;
contains([X | _], X) -> true;
contains([_ | T], X) -> contains(T, X).

duplicateElements([]) -> [];
duplicateElements([H | T]) -> [H, H] ++ duplicateElements(T).

sumFloats([]) -> 0.0;
sumFloats([H | T]) -> H + sumFloats(T).

sumRecursively([], ACC) -> ACC;
sumRecursively([H | T], ACC) -> sumRecursively(T, H + ACC).

sumFloatsTail(L) -> sumRecursively(L, 0).

