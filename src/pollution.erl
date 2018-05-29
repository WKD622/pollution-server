%%%-------------------------------------------------------------------
%%% @author Jakub Ziarko
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 18. Apr 2018 11:01
%%%-------------------------------------------------------------------
-module(pollution).
-author("jakub").

%% API
-export([createMonitor/0, addStation/3, addValue/5, removeValue/4, getOneValue/4, getStationMean/3, getDailyMean/3, getOverLimit/2, main/0]).

-record(measurement, {date, pollution = "", val = ""}).
-record(station, {name = "", coordinates = {0, 0}, measurements = []}).

main() ->
  P = createMonitor(),
  P1 = addStation("Aleja Slowackiego", {12.34, 56.78}, P),
  P2 = addStation("Starowislna", {43.12, 87.65}, P1),
  P3 = addValue("Aleja Slowackiego", calendar:local_time(), "PM10", 59, P2),
  P4 = addValue("Starowislna", "2018,05,24", "PM25", 62, P3),
  P5 = addValue("Starowislna", calendar:local_time(), "PM10", 59, P4),
  getOneValue("Starowislna", "2018,05,24", "PM25", P5).
% P5 = removeValue("Starowislna", calendar:local_time(), "PM25", P4).

%% Creates new monitor
createMonitor() -> [].

%% Adds station
addStation(Name, Coordinates, Monitor) ->
  case findStation(Name, Monitor) of
    true -> {error, "There is already station with this name"};
    false -> case findStation(Coordinates, Monitor) of
               true -> {error, "There is already stations with this coordinates"};
               false -> [#station{name = Name, coordinates = Coordinates} | Monitor]
             end
  end.

%% Adds measurements
addValue(Station, Date, Type, Value, Monitor) ->
  case findStation(Station, Monitor) of
    true -> insertValue(Station, Date, Type, Value, Monitor);
    false -> {error, "There is no such station"}
  end.

%% Removes value from choosen Monitor
removeValue(Station, Date, Type, Monitor) ->
  case findStation(Station, Monitor) of
    true -> doRemove(Station, Date, Type, Monitor);
    false -> {error, "There is no such station"}
  end.

% Returns value of choosen type, from choosen date and station
getOneValue(_, _, _, []) -> [];
getOneValue(Station, Data, Type, [#station{name = Station, measurements = L} | _]) ->
  getOneValueFromStation(Type, Data, L);
getOneValue(Station, Data, Type, [_ | T]) -> getOneValue(Station, Data, Type, T).

%returns average value of choosen parameter in choosen station
getStationMean(_, _, []) -> {error, "There is no such measurement"};
getStationMean(Station, Type, [#station{name = Station, measurements = L} | _]) -> getAverage(Type, L, 0, 0);
getStationMean(Station, Type, [_ | T]) -> getStationMean(Station, Type, T).

%returns average value of choosen parameter in choosen date from all stations
getDailyMean(Type, {Day, Month, Year}, Monitor) -> averageDailyMonitor(Type, {Day, Month, Year}, {0, 0}, Monitor).

getOverLimit(Count, []) -> Count;
getOverLimit(_, [#station{measurements = M} | T]) -> getOverLimit(measurementOverLimit(M), T).

%% INTERNAL FUNCTIONS
findStation(_, []) -> false;
findStation(Name, [#station{name = Name} | _]) -> true;
findStation(Coordinates, [#station{coordinates = Coordinates} | _]) -> true;
findStation(Element, [_ | T]) -> findStation(Element, T).

findMeasurement(Date, Type, [#measurement{date = Date, pollution = Type} | _]) -> true;
findMeasurement(Date, Type, [_ | T]) -> findMeasurement(Date, Type, T);
findMeasurement(_, _, []) -> false.

insertValue(_, _, _, _, []) -> [];
insertValue(Station, Data, Type, Value, [#station{name = Station, measurements = L} = S | T]) ->
  case findMeasurement(Data, Type, L) of
    true -> {error, "That measure already exists"};
    false -> [S#station{measurements = [#measurement{date = Data, pollution = Type, val = Value} | L]} | T]
  end;
insertValue(Station, Data, Type, Value, [H | T]) -> [H] ++ insertValue(Station, Data, Type, Value, T).

doRemove(_, _, _, []) -> [];
doRemove(Station, Date, Type, [#station{name = Station, measurements = L} = S | T]) ->
  case findMeasurement(Date, Type, L) of
    false -> {error, "There is no such measurement"};
    true -> [S#station{measurements = remove(Date, Type, L)} | T]
  end;
doRemove(Station, Date, Type, [#station{coordinates = Station, measurements = L} = S | T]) ->
  case findMeasurement(Date, Type, L) of
    false -> {error, "There is no such measurement"};
    true -> [S#station{measurements = remove(Date, Type, L)} | T]
  end;
doRemove(Station, Date, Type, [H | T]) -> [H | doRemove(Station, Date, Type, T)].

remove(Date, Type, [#measurement{date = Date, pollution = Type} | T]) -> T;
remove(Date, Type, [H | T]) -> [H] ++ remove(Date, Type, T);
remove(_, _, []) -> [].

getOneValueFromStation(_, _, []) -> {error, "There is no such measure"};
getOneValueFromStation(Type, Date, [#measurement{pollution = Type, date = Date} = M | _]) -> M#measurement.val;
getOneValueFromStation(Type, Date, [_ | T]) -> getOneValueFromStation(Type, Date, T).

getAverage(_, [], _, 0) -> 0;
getAverage(_, [], Sum, Count) -> Sum / Count;
getAverage(Type, [#measurement{pollution = Type, val = V} | T], Sum, Count) -> getAverage(Type, T, Sum + V, Count + 1);
getAverage(Type, [_ | T], Sum, Count) -> getAverage(Type, T, Sum, Count).

averageDailyMonitor(_, _, {0, 0}, []) -> {error, "There is no such parameter"};
averageDailyMonitor(_, _, {Sum, Count}, []) -> Sum / Count;
averageDailyMonitor(Type, {Day, Month, Year}, {Sum, Count}, [#station{measurements = M} | T]) ->
  averageDailyMonitor(Type, {Day, Month, Year}, sum({Sum, Count}, averageDailyStation(Type, {Day, Month, Year}, M, Sum, Count)), T).

averageDailyStation(_, _, [], Sum, Count) -> {Sum, Count};
averageDailyStation(Type, {Day, Month, Year}, [#measurement{pollution = Type, date = {{Day, Month, Year}, {_, _, _}}, val = V} | T], Sum, Count) ->
  averageDailyStation(Type, {Day, Month, Year}, T, Sum + V, Count + 1);
averageDailyStation(Type, {Year, Month, Day}, [_ | T], Sum, Count) ->
  averageDailyStation(Type, {Day, Month, Year}, T, Sum, Count).

sum({A, B}, {C, D}) -> {A + C, B + D}.

greater(X, Y) ->
  if
    X > Y -> 1;
    X =< Y -> 0
  end.

measurementOverLimit([]) -> 0;
measurementOverLimit([#measurement{pollution = "PM10", val = V} | _]) -> greater(V, 50);
measurementOverLimit([#measurement{pollution = "PM25", val = V} | _]) -> greater(V, 30);
measurementOverLimit([_ | T]) -> measurementOverLimit(T).