-module(calculator).
-import(lists, [lenght/1]).

%% API
-export([generic_data/0, number_of_readings/2, calculate_max/2, calculate_mean/2]).

number_of_readings([], _) -> 0;
number_of_readings([H | T], Date) ->
  {Sample_date, _} = erlang:element(2, H),
  Cnt =
    if Sample_date == Date -> length(erlang:element(3, H));
      true -> 0
    end,
  Cnt + number_of_readings(T, Date).

calculate_max([], _) -> none;
calculate_max([H | T], Type) ->
  Head_max = get_max_from_one_reading(erlang:element(3, H), Type),
  Tail_max = calculate_max(T, Type),
  case {Head_max, Tail_max} of
    {none, none} -> none;
    {none, TMax} -> TMax;
    {HMax, none} -> HMax;
    {HMax, TMax} -> erlang:max(HMax, TMax)
  end.

get_max_from_one_reading([], _) -> none;
get_max_from_one_reading([H | T], Type) ->
  Head_value =
    if
      Type == erlang:element(1, H) -> erlang:element(2, H);
      true -> none
    end,
  Tail_value = get_max_from_one_reading(T, Type),
  case {Head_value, Tail_value} of
    {none, none} -> none;
    {none, TMax} -> TMax;
    {HMax, none} -> HMax;
    {HMax, TMax} -> erlang:max(HMax, TMax)
  end.

calculate_mean(Readings, Type) ->
  Sum = get_sum(Readings, Type),
  Counter = get_readings_count(Readings, Type),
  case {Sum, Counter} of
    {none, _} -> none;
    {_, 0} -> none;
    {Sum, Cnt} -> Sum / Cnt
  end.

get_sum([], _) -> none;
get_sum([H | T], Type) ->
  Head_sum = get_sum_from_one_reading(erlang:element(3, H), Type),
  Tail_sum = get_sum(T, Type),
  case {Head_sum, Tail_sum} of
    {none, none} -> none;
    {Hs, none} -> Hs;
    {none, Ts} -> Ts;
    {Hs, Ts} -> Hs + Ts
  end.

get_sum_from_one_reading([], _) -> none;
get_sum_from_one_reading([H | T], Type) ->
  Head_value =
    if
      erlang:element(1, H) == Type -> erlang:element(2, H);
      true -> none
    end,
  Tail_value = get_sum_from_one_reading(T, Type),
  case {Head_value, Tail_value} of
    {none, none} -> none;
    {Hv, none} -> Hv;
    {none, Tv} -> Tv;
    {Hv, Tv} -> Hv + Tv
  end.

get_readings_count([], _) -> 0;
get_readings_count([H | T], Type) ->
  get_count_from_one_reading(erlang:element(3, H), Type) + get_readings_count(T, Type).

get_count_from_one_reading([], _) -> 0;
get_count_from_one_reading([H | T], Type) ->
  Head_value =
    if
      erlang:element(1, H) == Type -> 1;
      true -> 0
    end,
  Tail_value = get_count_from_one_reading(T, Type),
  Head_value + Tail_value.

generic_data() -> [
  {
    "Cracow_AGH",
    {{2025,3,6}, erlang:time()},
    [
      {"PM10", 22},
      {"PM2.5", 18.7},
      {"PM1", 13.2}
    ]
  },
  {
    "Cracow_OldTown",
    {erlang:date(), erlang:time()},
    [
      {"PM10", 20},
      {"PM2.5", 18.7},
      {"Temperature", 25.2}
    ]
  },
  {
    "Cracow_Bronowice",
    {{2025,3,1}, erlang:time()},
    [
      {"PM10", 20},
      {"PM1", 13.2},
      {"Temperature", 22.1},
      {"Humidity", 34}
    ]
  },
  {
    "Cracow_Krowodrza",
    {erlang:date(), erlang:time()},
    [
      {"PM10", 22},
      {"PM2.5", 18.7},
      {"PM1", 13.2},
      {"Humidity", 40.1}
    ]
  }
].