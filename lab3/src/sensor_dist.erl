-module(sensor_dist).
-author("arima").

%% API
-export([get_rand_locations/1, find_closest/2, find_closest_parallel/2]).

get_rand_locations(Number) ->
  [{rand:uniform(10000), rand:uniform(10000)} || _ <- lists:seq(1, Number)].

dist({X1, Y1}, {X2, Y2}) ->
  math:sqrt((X1 - X2) * (X1 - X2) + (Y1 - Y2) * (Y1 - Y2)).

find_for_person(PersonLocation, SensorsLocations) ->
  lists:min([{dist(PersonLocation, SL), {PersonLocation, SL}} || SL <- SensorsLocations]).

find_closest(PeopleLocations, SensorLocations) ->
  lists:min([find_for_person(PL, SensorLocations) || PL <- PeopleLocations]).

find_for_person(PersonLocation, SensorsLocations, ParentPID) ->
  ParentPID ! find_for_person(PersonLocation, SensorsLocations).

find_closest_parallel(PeopleLocations, SensorLocations) ->
  PIDs = [spawn(?MODULE, fun find_for_person/3, [PL, SensorLocations, self()]) || PL <- PeopleLocations],
  lists:min([receive D -> D end || _ <- PIDs]).
