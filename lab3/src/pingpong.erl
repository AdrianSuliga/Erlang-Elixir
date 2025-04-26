-module(pingpong).
-author("arima").

%% API
-export([start/0, play/1, stop/0]).

start() ->
  register(ping_process, spawn(fun() -> ping(0) end)),
  register(pong_process, spawn(fun() -> pong() end)).

play(N) ->
  ping_process ! N.

stop() ->
  ping_process ! stop,
  pong_process ! stop.

ping(S) ->
  receive
    stop ->
      ok;
    0 ->
      io:format("Ping ~w, Sum ~w~n", [0, S]),
      ping(S);
    N ->
      io:format("Ping ~w, Sum ~w~n", [N, S + N]),
      timer:sleep(1000),
      pong_process ! (N - 1),
      ping(S + N)
  after 20000 ->
    timeout
  end.

pong() ->
  receive
    stop ->
      ok;
    0 ->
      pong();
    N ->
      io:format("Pong ~w~n", [N]),
      timer:sleep(1000),
      ping_process ! (N - 1),
      pong()
  after 20000 ->
    timeout
  end.