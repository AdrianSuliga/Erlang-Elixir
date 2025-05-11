defmodule PollutionDataLoader do
  def parseLine(line) do
    [date_and_time, type, value, stationId, name, coordinates] = String.split(line, ";")

    date =
      String.slice(date_and_time, 0, 10)
      |> String.split("-")
      |> Enum.map(&String.to_integer/1)
      |> List.to_tuple()

    time =
      String.slice(date_and_time, 11, 8)
      |> String.split(":")
      |> Enum.map(&String.to_integer/1)
      |> List.to_tuple()

    location =
      String.split(coordinates, ",")
      |> Enum.map(&String.to_float/1)
      |> List.to_tuple()

    %{
      datetime: {date, time},
      location: location,
      stationId: String.to_integer(stationId),
      stationName: name,
      pollutionType: type,
      pollutionLevel: String.to_float(value)
    }
  end

  def identifyStations(list_of_maps) do
    list_of_maps
    |> Enum.map(fn map ->
      %{
        stationId: map.stationId,
        stationName: map.stationName,
        location: map.location
      }
    end)
    |> Enum.uniq_by(fn data ->
      {
        data.stationId,
        data.stationName,
        data.location
      }
    end)
  end

  # .csv file -> list of maps
  def parse(path) do
    File.read!(path)
    |> String.trim()
    |> String.split("\n")
    |> Enum.map(&parseLine/1)
  end

  # list of maps -> saving data to server
  def load(list_of_data) do
    unique_list_of_stations = identifyStations(list_of_data)

    # Run pollution gen server
    Code.append_path("/home/arima/Studia/Semestr_4/Erlang-Elixir/lab4/_build/default/lib/lab4/ebin")
    case Application.start(:lab4) do
      :ok ->
        IO.puts("lab4 started successfully")

        # Add stations to the server
        {time_station, _} = :timer.tc(fn ->
          Enum.each(
            unique_list_of_stations,
            fn %{stationId: id, stationName: name, location: location} ->
              :pollution_gen_server.add_station("#{id} #{name}", location)
            end
          )
        end)
        IO.puts("added unique stations to the server [#{time_station / 1000000}]")

        # Add measurements to the server
        {time_measurements, _} = :timer.tc(fn ->
          Enum.each(
            list_of_data,
            fn %{
              datetime: date,
              location: id,
              stationId: _sid,
              stationName: _name,
              pollutionType: type,
              pollutionLevel: value
            } ->
              :pollution_gen_server.add_value(id, date, type, value)
            end
          )
        end)
        IO.puts("added measurements [#{time_measurements / 1000000}]")
      {:error, _} ->
        IO.puts("Failed to start :lab4")
    end
  end
end
