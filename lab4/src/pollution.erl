-module(pollution).
-author("arima").

%% API
-export([create_monitor/0, add_station/3, add_value/5, remove_value/4,
  get_one_value/4, get_station_min/3, get_daily_mean/3, get_maximum_variation_station/2]).

create_monitor() -> [].

add_station(Name, Coordinates, Monitor) ->
  NameExists = length(lists:filter(fun(X) -> {N, _, _} = X, N == Name end, Monitor)) =/= 0,
  CoordsExists = length(lists:filter(fun(X) -> {_, C, _} = X, C == Coordinates end, Monitor)) =/= 0,

  case {NameExists, CoordsExists} of
    {true, _} -> {error, "Station with this name already exists"};
    {_, true} -> {error, "Station with this coordinates already exists"};
    {false, false} -> [{Name, Coordinates, []} | Monitor]
  end.

add_value(Identifier, Date, Type, Value, Monitor) ->
  case station_exists(Identifier, Monitor) of
    false -> {error, "This station does not exist"};
    true ->
      StationData = station_data(Identifier, Monitor),
      {_, _, CurrentReadings} = StationData,
      ReadingExists = length(lists:filter(fun({D, T, _}) -> D == Date andalso T == Type end, CurrentReadings)) =/= 0,

      case ReadingExists of
        true -> {error, "There is already reading with this date and type on this station"};
        false ->
          NewReadings = [{Date, Type, Value} | CurrentReadings],
          {StationName, StationCoords, _} = StationData,
          NewStationData = {StationName, StationCoords, NewReadings},
          replace_list_element(StationData, NewStationData, Monitor)
      end
  end.

remove_value(Identifier, Date, Type, Monitor) ->
  case station_exists(Identifier, Monitor) of
    false -> {error, "This station does not exist"};
    true ->
      StationData = station_data(Identifier, Monitor),
      {_, _, CurrentReadings} = StationData,
      ReadingExists = length(lists:filter(fun({D, T, _}) -> D == Date andalso T == Type end, CurrentReadings)) =/= 0,

      case ReadingExists of
        false -> {error, "There is no reading to remove"};
        true ->
          NewReadings = lists:filter(fun({D, T, _}) -> D =/= Date orelse T =/= Type end, CurrentReadings),
          {StationName, StationCoords, _} = StationData,
          NewStationData = {StationName, StationCoords, NewReadings},
          replace_list_element(StationData, NewStationData, Monitor)
      end
  end.

get_one_value(Identifier, Date, Type, Monitor) ->
  case station_exists(Identifier, Monitor) of
    false -> {error, "This station does not exist"};
    true ->
      StationData = station_data(Identifier, Monitor),
      {_, _, CurrentReadings} = StationData,
      Reading = lists:filter(fun({D, T, _}) -> D == Date andalso T == Type end, CurrentReadings),

      case Reading of
        [] -> {error, "No readings found"};
        _ ->
          [{_, _, Result}] = Reading,
          Result
      end
  end.

get_station_min(Identifier, Type, Monitor) ->
  case station_exists(Identifier, Monitor) of
    false -> {error, "This station does not exist"};
    true ->
      StationData = station_data(Identifier, Monitor),
      {_, _, CurrentReadings} = StationData,
      SelectedReadings = lists:foldl(fun({_, T, V}, Acc) -> case T of Type -> [V | Acc]; _ -> Acc end end, [], CurrentReadings),
      case SelectedReadings of
        [] -> {error, "No readings with this type found"};
        _ -> lists:min(SelectedReadings)
      end
  end.

get_daily_mean(Type, Date, Monitor) ->
  AllReadings = lists:foldl(fun({_, _, Readings}, Acc) -> Readings ++ Acc end, [], Monitor),
  ReadingsToConsider = lists:filter(fun({{D, _}, T, _}) -> T == Type andalso D == Date end, AllReadings),
  ValuesToConsider = lists:foldl(fun({_, _, Value}, Acc) -> [Value | Acc] end, [], ReadingsToConsider),
  case ValuesToConsider of
    [] -> {error, "No readings found"};
    _ -> lists:sum(ValuesToConsider) / length(ValuesToConsider)
  end.

get_maximum_variation_station(Type, Monitor) ->
  Result = lists:map(fun(StationData) -> get_variation_from_station(StationData, Type) end, Monitor),
  MaxVariation = lists:max([Variation || {_, _, Variation} <- Result]),
  ListOfMaxVars = [{Name, Coords, Variation} || {Name, Coords, Variation} <- Result, Variation == MaxVariation],
  [OneOfMaxVarStations] =  ListOfMaxVars,
  OneOfMaxVarStations.

get_variation_from_station({Name, Coords, Readings}, Type) ->
  ConsideredReadings = lists:filter(fun({_, T, _}) -> T == Type end, Readings),
  Values = [V || {_, _, V} <- ConsideredReadings],
  Variation = case Values of
                [] -> 0;
                _ -> lists:max(Values) - lists:min(Values)
              end,
  {Name, Coords, Variation}.

station_exists({X,Y}, Monitor) -> length(lists:filter(fun({_, C, _}) -> C == {X,Y} end, Monitor)) =/= 0;
station_exists(Name, Monitor) -> length(lists:filter(fun({N, _, _}) -> N == Name end, Monitor)) =/= 0.

station_data({X,Y}, Monitor) ->
  [R] = lists:filter(fun(Z) -> case Z of {_, {X, Y}, _} -> true; _ -> false end end, Monitor), R;
station_data(Name, Monitor) ->
  [R] = lists:filter(fun(Z) -> case Z of {Name, _, _} -> true; _ -> false end end, Monitor), R.

replace_list_element(_, _, []) -> [];
replace_list_element(Old, New, [Old | T]) -> [New | T];
replace_list_element(Old, New, [H | T]) -> [H | replace_list_element(Old, New, T)].