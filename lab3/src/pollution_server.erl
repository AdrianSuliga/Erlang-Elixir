-module(pollution_server).
-author("arima").

-export([start/0, stop/0, add_station/2, add_value/4, remove_value/3,
  get_one_value/3, get_station_min/2, get_daily_mean/2, get_maximum_variation_station/1]).

init() ->
  pollution:create_monitor().

start() ->
  register(server_process, spawn(fun() -> loop(init()) end)),
  {ok, "Server started"}.

stop() ->
  case whereis(server_process) of
    undefined -> ok;
    P ->
      P ! {stop, "Stopping server"},
      unregister(server_process)
  end,
  {ok, "Server stopped"}.

loop(Monitor) ->
  receive
    {add_station, Caller, Name, Coords} ->

      NewMonitor = pollution:add_station(Name, Coords, Monitor),
      Caller ! NewMonitor,
      case NewMonitor of
        {error, _} -> loop(Monitor);
        _ -> loop(NewMonitor)
      end;

    {add_value, Caller, Identifier, Data, Type, Value} ->

      NewMonitor = pollution:add_value(Identifier, Data, Type, Value, Monitor),
      Caller ! NewMonitor,
      case NewMonitor of
        {error, _} -> loop(Monitor);
        _ -> loop(NewMonitor)
      end;

    {remove_value, Caller, Identifier, Data, Type} ->

      NewMonitor = pollution:remove_value(Identifier, Data, Type, Monitor),
      Caller ! NewMonitor,
      case NewMonitor of
        {error, _} -> loop(Monitor);
        _ -> loop(NewMonitor)
      end;

    {get_one_value, Caller, Identifier, Date, Type} ->

      Result = pollution:get_one_value(Identifier, Date, Type, Monitor),
      Caller ! Result,
      loop(Monitor);

    {get_station_min, Caller, Identifier, Type} ->

      Result = pollution:get_station_min(Identifier, Type, Monitor),
      Caller ! Result,
      loop(Monitor);

    {get_daily_mean, Caller, Type, Date} ->

      Result = pollution:get_daily_mean(Type, Date, Monitor),
      Caller ! Result,
      loop(Monitor);

    {get_maximum_variation_station, Caller, Type} ->

      Result = pollution:get_maximum_variation_station(Type, Monitor),
      Caller ! Result,
      loop(Monitor);

    {stop, Caller, Message} ->

      Caller ! Message,
      {stop, Message}

  end.

add_station(Name, Coords) ->
  server_process ! {add_station, self(), Name, Coords},
  receive
    Result -> Result
  end.

add_value(Identifier, Data, Type, Value) ->
  server_process ! {add_value, self(), Identifier, Data, Type, Value},
  receive
    Result -> Result
  end.

remove_value(Identifier, Data, Type) ->
  server_process ! {remove_value, self(), Identifier, Data, Type},
  receive
    Result -> Result
  end.

get_one_value(Identifier, Date, Type) ->
  server_process ! {get_one_value, self(), Identifier, Date, Type},
  receive
    Result -> Result
  end.

get_station_min(Identifier, Type) ->
  server_process ! {get_station_min, self(), Identifier, Type},
  receive
    Result -> Result
  end.

get_daily_mean(Type, Date) ->
  server_process ! {get_daily_mean, self(), Type, Date},
  receive
    Result -> Result
  end.

get_maximum_variation_station(Type) ->
  server_process ! {get_maximum_variation_station, self(), Type},
  receive
    Result -> Result
  end.