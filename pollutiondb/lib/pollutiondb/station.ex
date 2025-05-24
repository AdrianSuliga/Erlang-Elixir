defmodule Pollutiondb.Station do
  require Ecto.Query
  use Ecto.Schema

  schema "stations" do
    field :name, :string
    field :lon, :float
    field :lat, :float
    has_many :readings, Pollutiondb.Reading
  end

  def add(station) do
    Pollutiondb.Repo.insert(station)
  end

  def changeset(station, changesmap) do
    station
    |> Ecto.Changeset.cast(changesmap, [:name, :lon, :lat])
    |> Ecto.Changeset.validate_required([:name, :lon, :lat])
    |> Ecto.Changeset.validate_number(:lon, greater_than_or_equal_to: -180.0, less_than_or_equal_to: 180.0)
    |> Ecto.Changeset.validate_number(:lat, greater_than_or_equal_to: -90.0, less_than_or_equal_to: 90.0)
  end

  def add(name, lon, lat) do
    %__MODULE__{}
    |> changeset(%{name: name, lon: lon, lat: lat})
    |> add
  end

  def get_all() do
    Pollutiondb.Repo.all(Pollutiondb.Station)
    |> Pollutiondb.Repo.preload(:readings)
  end

  def get_by_id(id) do
    Pollutiondb.Repo.get(Pollutiondb.Station, id)
    |> Pollutiondb.Repo.preload(:readings)
  end

  def remove(station) do
    Pollutiondb.Repo.delete(station)
  end

  def find_by_name(name) do
    Pollutiondb.Repo.all(
      Ecto.Query.where(Pollutiondb.Station, name: ^name)
    )
    |> Pollutiondb.Repo.preload(:readings)
  end

  def find_by_location(lon, lat) do
    Ecto.Query.from(
      s in Pollutiondb.Station,
      where: s.lon == ^lon,
      where: s.lat == ^lat
    )
    |> Pollutiondb.Repo.all
    |> Pollutiondb.Repo.preload(:readings)
  end

  def find_by_location_range(lon_min, lon_max, lat_min, lat_max) do
    Ecto.Query.from(
      s in Pollutiondb.Station,
      where: ^lon_min <= s.lon,
      where: s.lon <= ^lon_max,
      where: ^lat_min <= s.lat,
      where: s.lat <= ^lat_max
    )
    |> Pollutiondb.Repo.all
    |> Pollutiondb.Repo.preload(:readings)
  end

  def update_name(station, new_name) do
    changeset(station, %{name: new_name})
    |> Pollutiondb.Repo.update()
  end
end