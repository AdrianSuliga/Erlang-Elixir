%%%-------------------------------------------------------------------
%%% @author arima
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 26. Apr 2025 11:35 PM
%%%-------------------------------------------------------------------
-module(pollution_value_collector_gen_statem).
-author("arima").

-behaviour(gen_statem).

%% API
-export([start_link/0, set_station/1, add_value/3, store_data/0]).

%% gen_statem callbacks
-export([init/1, callback_mode/0, format_status/2, wait_for_station/3, wait_for_value/3, terminate/3, code_change/4]).

-define(SERVER, ?MODULE).

-record(pollution_value_collector_gen_statem_state, {
  machine_state = {}
}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Creates a gen_statem process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
start_link() ->
  gen_statem:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================

init([]) ->
  {ok, wait_for_station, #pollution_value_collector_gen_statem_state{}}.

callback_mode() ->
  state_functions.

format_status(_Opt, [_PDict, _StateName, _State]) ->
  Status = some_term,
  Status.

wait_for_station(
    _EventType,
    {set_station, Identifier},
    State = #pollution_value_collector_gen_statem_state{}) ->
  NewState = {Identifier, []},
  {next_state, wait_for_value, State#pollution_value_collector_gen_statem_state{machine_state = NewState}}.

wait_for_value(
    _EventType,
    {add_value, Date, Type, Value},
    State = #pollution_value_collector_gen_statem_state{machine_state = ExtractedState}) ->
  {Id, Vs} = ExtractedState,
  NewV = Vs ++ [{Date, Type, Value}],
  NewState = {Id, NewV},
  {next_state, wait_for_value, State#pollution_value_collector_gen_statem_state{machine_state = NewState}};

wait_for_value(
    _EventType,
    store_data,
    State = #pollution_value_collector_gen_statem_state{machine_state = ExtractedState}) ->
  {Id, Values} = ExtractedState,
  lists:map(fun({D, T, V}) -> pollution_gen_server:add_value(Id, D, T, V) end, Values),
  {next_state, wait_for_station, State#pollution_value_collector_gen_statem_state{machine_state = {}}}.

terminate(_Reason, _StateName, _State = #pollution_value_collector_gen_statem_state{}) ->
  ok.

code_change(_OldVsn, StateName, State = #pollution_value_collector_gen_statem_state{}, _Extra) ->
  {ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

set_station(Identifier) ->
  gen_statem:cast(?SERVER, {set_station, Identifier}).

add_value(Date, Type, Value) ->
  gen_statem:cast(?SERVER, {add_value, Date, Type, Value}).

store_data() ->
  gen_statem:cast(?SERVER, store_data).