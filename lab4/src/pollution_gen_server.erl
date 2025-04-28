%%%-------------------------------------------------------------------
%%% @author arima
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(pollution_gen_server).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

-export([add_station/2, add_value/4, remove_value/3, get_one_value/3,
  get_station_min/2, get_daily_mean/2, get_maximum_variation_station/1,
  crash/0, stop/0]).

-define(SERVER, ?MODULE).

-record(pollution_gen_server_state, {
  pollution_state = pollution:create_monitor()
}).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
  {ok, #pollution_gen_server_state{}}.

handle_call(
    {get_one_value, Identifier, Date, Type},
    _From,
    State = #pollution_gen_server_state{pollution_state = ExtractedState}) ->
  {reply, pollution:get_one_value(Identifier, Date, Type, ExtractedState), State};

handle_call(
    {get_station_min, Identifier, Type},
    _From,
    State = #pollution_gen_server_state{pollution_state = ExtractedState}) ->
  {reply, pollution:get_station_min(Identifier, Type, ExtractedState), State};

handle_call(
    {get_daily_mean, Type, Date},
    _From,
    State = #pollution_gen_server_state{pollution_state = ExtractedState}) ->
  {reply, pollution:get_daily_mean(Type, Date, ExtractedState), State};

handle_call(
    {get_maximum_variation_station, Type},
    _From,
    State = #pollution_gen_server_state{pollution_state = ExtractedState}) ->
  {reply, pollution:get_maximum_variation_station(Type, ExtractedState), State}.

handle_cast(
    {add_station, Name, Coordinates},
    State = #pollution_gen_server_state{pollution_state = ExtractedState}) ->
  NewState = pollution:add_station(Name, Coordinates, ExtractedState),
  {noreply, State#pollution_gen_server_state{pollution_state = NewState}};

handle_cast(
    {add_value, Identifier, Date, Type, Value},
    State = #pollution_gen_server_state{pollution_state = ExtractedState}) ->
  NewState = pollution:add_value(Identifier, Date, Type, Value, ExtractedState),
  {noreply, State#pollution_gen_server_state{pollution_state = NewState}};

handle_cast(
    {remove_value, Identifier, Date, Type},
    State = #pollution_gen_server_state{pollution_state = ExtractedState}) ->
  NewState = pollution:remove_value(Identifier, Date, Type, ExtractedState),
  {noreply, State#pollution_gen_server_state{pollution_state = NewState}}.

handle_info(_Info, State = #pollution_gen_server_state{}) ->
  {noreply, State}.

terminate(_Reason, _State = #pollution_gen_server_state{}) ->
  ok.

code_change(_OldVsn, State = #pollution_gen_server_state{}, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

get_one_value(Identifier, Date, Type) ->
  gen_server:call(?SERVER, {get_one_value, Identifier, Date, Type}).

get_station_min(Identifier, Type) ->
  gen_server:call(?SERVER, {get_station_min, Identifier, Type}).

get_daily_mean(Type, Date) ->
  gen_server:call(?SERVER, {get_daily_mean, Type, Date}).

get_maximum_variation_station(Type) ->
  gen_server:call(?SERVER, {get_maximum_variation_station, Type}).

add_station(Name, Coordinates) ->
  gen_server:cast(?SERVER, {add_station, Name, Coordinates}).

add_value(Identifier, Date, Type, Value) ->
  gen_server:cast(?SERVER, {add_value, Identifier, Date, Type, Value}).

remove_value(Identifier, Date, Type) ->
  gen_server:cast(?SERVER, {remove_value, Identifier, Date, Type}).

crash() ->
  1 / 0.

stop() ->
  gen_server:stop(?SERVER).
