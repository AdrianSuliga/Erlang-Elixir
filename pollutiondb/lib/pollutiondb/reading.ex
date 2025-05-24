defmodule Pollutiondb.Reading do
  require Ecto.Query
  use Ecto.Schema

  schema "readings" do
    field :date, :date
    field :time, :time
    field :type, :string
    field :value, :float
    belongs_to :station, Pollutiondb.Station
  end

  def add(station, date, time, type, value) do
    {year, month, day} = date
    {hour, min, sec} = time

    s_date = Date.new!(year, month, day)
    s_time = Time.new!(hour, min, sec)

    %Pollutiondb.Reading{}
    |> Ecto.Changeset.cast(%{
      date: s_date,
      time: s_time,
      type: type,
      value: value
    }, [:date, :time, :type, :value])
    |> Ecto.Changeset.put_assoc(:station, station)
    |> Ecto.Changeset.validate_required([:date, :time, :type, :value, :station])
    |> Pollutiondb.Repo.insert()
  end

  def add_now(station, type, value) do
    %Pollutiondb.Reading{}
    |> Ecto.Changeset.cast(%{
      date: Date.utc_today(),
      time: Time.utc_now(),
      type: type,
      value: value
    }, [:date, :time, :type, :value])
    |> Ecto.Changeset.put_assoc(:station, station)
    |> Ecto.Changeset.validate_required([:date, :time, :type, :value, :station])
    |> Pollutiondb.Repo.insert()
  end

  def get_all() do
    Pollutiondb.Repo.all(Pollutiondb.Reading)
    |> Pollutiondb.Repo.preload(:station)
  end

  def get_by_id(reading_id) do
    Pollutiondb.Repo.get(Pollutiondb.Reading, reading_id)
    |> Pollutiondb.Repo.preload(:station)
  end

  # date is {{DD,MM,YYYY}, {HH,MM,SS}}
  def find_by_date(date) do
    {{year, month, day}, {hour, min, sec}} = date
    s_date = Date.new!(year, month, day)
    s_time = Time.new!(hour, min, sec)

    Ecto.Query.from(
      r in Pollutiondb.Reading,
      where: ^s_date == r.date,
      where: ^s_time == r.time
    )
    |> Pollutiondb.Repo.all
    |> Pollutiondb.Repo.preload(:station)
  end

  def remove(reading) do
    Pollutiondb.Repo.delete(reading)
  end
end