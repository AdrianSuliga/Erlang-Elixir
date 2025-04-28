%%%-------------------------------------------------------------------
%%% @author Wojciech Turek
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 14. mar 2019 12:50
%%%-------------------------------------------------------------------
-module(pollution_test).
-author("Wojciech Turek").

-include_lib("eunit/include/eunit.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
create_server_test() ->
  {M, _} = pollution_gen_server:start_link(),
  ?assertMatch(ok, M),
  pollution_gen_server:stop().

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_one_value_test() ->
  pollution_gen_server:start_link(),
  pollution_gen_server:add_station("Stacja 1", {1,1}),
  Time = calendar:local_time(),
  pollution_gen_server:add_value("Stacja 1", Time, "PM10", 46.3),
  pollution_gen_server:add_value("Stacja 1", Time, "PM1", 36.3),
  pollution_gen_server:add_value("Stacja 1", {{2023,3,27},{11,16,9}}, "PM10", 26.3),

  ?assertMatch(46.3, pollution_gen_server:get_one_value("Stacja 1", Time, "PM10")),
  ?assertMatch(36.3, pollution_gen_server:get_one_value("Stacja 1", Time, "PM1")),
  ?assertMatch(46.3, pollution_gen_server:get_one_value({1,1}, Time, "PM10")),
  ?assertMatch(26.3, pollution_gen_server:get_one_value("Stacja 1", {{2023,3,27},{11,16,9}}, "PM10")),
  pollution_gen_server:stop().

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_one_value_fail_test() ->
  pollution_gen_server:start_link(),
  pollution_gen_server:add_station("Stacja 1", {1,1}),
  Time = calendar:local_time(),
  pollution_gen_server:add_value("Stacja 1", Time, "PM10", 46.3),
  pollution_gen_server:add_value("Stacja 1", Time, "PM1", 36.3),
  pollution_gen_server:add_value("Stacja 1", {{2023,3,27},{11,16,9}}, "PM10", 26.3),

  ?assertMatch({error, _}, pollution_gen_server:get_one_value("Stacja 1", Time, "PM25")),
  ?assertMatch({error, _}, pollution_gen_server:get_one_value({1,1}, Time, "PM25")),
  ?assertMatch({error, _}, pollution_gen_server:get_one_value("Stacja 1", {{2023,3,27},{11,16,10}}, "PM10")),
  ?assertMatch({error, _}, pollution_gen_server:get_one_value("Stacja 2", Time, "PM1")),
  ?assertMatch({error, _}, pollution_gen_server:get_one_value({1,2}, Time, "PM10")),
  pollution_gen_server:stop().

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_station_min_test() ->
  pollution_gen_server:start_link(),
  pollution_gen_server:add_station("Stacja 1", {1,1}),
  pollution_gen_server:add_value("Stacja 1", {{2023,3,27},{11,16,10}}, "PM10", 10),
  pollution_gen_server:add_value("Stacja 1", {{2023,3,27},{11,16,11}}, "PM10", 20),
  ?assertMatch(10, pollution_gen_server:get_station_min("Stacja 1", "PM10")),

  pollution_gen_server:add_value("Stacja 1", {{2023,3,27},{11,16,12}}, "PM10", 5),
  ?assertMatch(5, pollution_gen_server:get_station_min({1,1}, "PM10")),

  pollution_gen_server:add_value("Stacja 1", {{2023,3,27},{11,16,13}}, "PM10", 2.3),
  ?assertMatch(2.3, pollution_gen_server:get_station_min("Stacja 1", "PM10")),
  pollution_gen_server:stop().

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_station_min_fail_test() ->
  pollution_gen_server:start_link(),
  pollution_gen_server:add_station("Stacja 1", {1,1}),
  ?assertMatch({error, _}, pollution_gen_server:get_station_min("Stacja 1", "PM10")),
  pollution_gen_server:add_value("Stacja 1", {{2023,3,27},{11,16,10}}, "PM10", 10),
  ?assertMatch({error, _}, pollution_gen_server:get_station_min("Stacja 1", "PM25")),
  ?assertMatch({error, _}, pollution_gen_server:get_station_min("Stacja 2", "PM25")),
  pollution_gen_server:stop().

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_daily_mean_test() ->
  pollution_gen_server:start_link(),
  pollution_gen_server:add_station("Stacja 1", {1,1}),
  pollution_gen_server:add_station("Stacja 2", {2,2}),
  pollution_gen_server:add_station("Stacja 3", {3,3}),

  pollution_gen_server:add_value("Stacja 1", {{2023,3,27},{11,16,10}}, "PM10", 10),
  pollution_gen_server:add_value("Stacja 2", {{2023,3,27},{11,16,11}}, "PM10", 20),

  ?assertMatch(15.0, pollution_gen_server:get_daily_mean("PM10",{2023,3,27})),

  pollution_gen_server:add_value("Stacja 1", {{2023,3,27},{11,16,12}}, "PM10", 10),
  pollution_gen_server:add_value("Stacja 2", {{2023,3,27},{11,16,13}}, "PM10", 20),

  pollution_gen_server:add_value("Stacja 1", {{2023,3,27},{11,16,14}}, "PM25", 100),
  pollution_gen_server:add_value("Stacja 2", {{2023,3,27},{11,16,15}}, "PM25", 220),

  ?assertMatch(15.0, pollution_gen_server:get_daily_mean("PM10",{2023,3,27})),

  pollution_gen_server:add_value("Stacja 1", {{2023,3,28},{11,16,16}}, "PM10", 2000),
  pollution_gen_server:add_value("Stacja 2", {{2023,3,28},{11,16,17}}, "PM10", 3000),
  pollution_gen_server:add_value("Stacja 3", {{2023,3,27},{11,16,18}}, "PM10", 1234),

  ?assertMatch(258.8, pollution_gen_server:get_daily_mean("PM10",{2023,3,27})),
  pollution_gen_server:stop().

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_daily_mean_fail_test() ->
  pollution_gen_server:start_link(),
  pollution_gen_server:add_station("Stacja 1", {1,1}),
  pollution_gen_server:add_station("Stacja 2", {2,2}),

  ?assertMatch({error, _}, pollution_gen_server:get_daily_mean("PM10",{2023,3,27})),

  pollution_gen_server:add_value("Stacja 1", {{2023,3,27},{11,16,10}}, "PM10", 10),
  pollution_gen_server:add_value("Stacja 2", {{2023,3,27},{11,16,11}}, "PM10", 20),

  ?assertMatch({error, _}, pollution_gen_server:get_daily_mean("PM25",{2023,3,27})),
  ?assertMatch({error, _}, pollution_gen_server:get_daily_mean("PM10",{2023,3,29})),
  pollution_gen_server:stop().

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_maximum_variation_station_test() ->
  pollution_gen_server:start_link(),
  pollution_gen_server:add_station("Stacja 1", {1,1}),
  pollution_gen_server:add_station("Stacja 2", {2,2}),
  pollution_gen_server:add_station("Stacja 3", {3,3}),

  pollution_gen_server:add_value("Stacja 1", {{2023,3,27},{11,16,10}}, "PM10", 10),
  pollution_gen_server:add_value("Stacja 2", {{2023,3,27},{11,16,11}}, "PM10", 20),
  pollution_gen_server:add_value("Stacja 1", {{2023,3,27},{11,16,12}}, "PM10", 10),
  pollution_gen_server:add_value("Stacja 2", {{2023,3,27},{11,16,13}}, "PM10", 20),

  pollution_gen_server:add_value("Stacja 1", {{2023,3,27},{11,16,14}}, "PM25", 100),
  pollution_gen_server:add_value("Stacja 2", {{2023,3,27},{11,16,15}}, "PM25", 220),

  pollution_gen_server:add_value("Stacja 1", {{2023,3,28},{11,16,16}}, "PM10", 2000),
  pollution_gen_server:add_value("Stacja 2", {{2023,3,28},{11,16,17}}, "PM10", 3000),

  pollution_gen_server:add_value("Stacja 3", {{2023,3,27},{11,16,18}}, "PM10", 1234),
  pollution_gen_server:add_value("Stacja 2", {{2022,3,22}, {11,11,11}}, "PM25", 250),

  ?assertMatch({"Stacja 2", {2,2}, 2980}, pollution_gen_server:get_maximum_variation_station("PM10")),
  ?assertMatch({"Stacja 2", {2,2}, 30}, pollution_gen_server:get_maximum_variation_station("PM25")),
  pollution_gen_server:stop().