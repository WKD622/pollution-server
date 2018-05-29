defmodule PollutionData do
  @moduledoc false

  def loadModules do
    IEx.Helpers.c("pollution.erl")
    IEx.Helpers.c("otpPollutionServer.erl")
    IEx.Helpers.c("otpPollutionSupervisor.erl")
  end

  def importLInesFromCSV do
    list = File.read!("pollution.csv")
           |> String.split()
    parseToTuple(hd(list))
    Enum.map(list,
        fn(x) -> parseToTuple(x) end)
  end

  def parseToTuple(line) do
    [date, time, x, y, pollutionLvl] = String.split(line,",")
    date = String.split(date, "-") |> Enum.reverse |> Enum.map(fn x -> String.to_integer(x) end) |> List.to_tuple
    time = (String.split(time, ":") |> Enum.map(fn x -> String.to_integer(x) end)) ++ [0] |> List.to_tuple
    location = {x |> String.to_float, y |> String.to_float}
    pollutionLvl = pollutionLvl |> String.to_integer
    station = "station_" <> "#{x}" <> "_" <> "#{y}"
    %{:datetime => {date, time}, :location => location, :pollutionLevel => pollutionLvl, :station => station}
  end

  def identifyStation(list) do
    Enum.reduce(list, %{}, fn reading, acc -> Map.put(acc, reading.location, "station_#{elem(reading.location,0)}_#{elem(reading.location,1)}" ) end)
    pollution:ot
  end
end

