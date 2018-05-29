%%%-------------------------------------------------------------------
%%% @author jakub
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 09. May 2018 22:27
%%%-------------------------------------------------------------------
-module(normalPollutionServer).
-author("jakub").
-c("pollution").

%% API
-export([start/0, crash/0, run/1, stop/0]).

start() ->
  io:format("Server started succesfully! ~n"),
  register(pollutionServer, spawn_link(?MODULE, run, [pollution:createMonitor()])).


run(Monitor) ->
  io:format("Running!~n"),
  receive
    stop -> ok;
    {addStation, {Name, Coordinates}} ->
      run(pollution:addStation(Name, Coordinates, Monitor));

    {addValue, {Station, Date, Type, Value}} ->
      run(pollution:addValue(Station, Date, Type, Value, Monitor));

    {removeValue, {Station, Date, Type}} ->
      run(pollution:removeValue(Station, Date, Type, Monitor));

    {getOneValue, {Station, Data, Type}} ->
      run(pollution:getOneValue(Station, Data, Type, Monitor));

    {getStationMean, {Station, Type}} ->
      run(pollution:getStationMean(Station, Type, Monitor));

    {getDailyMean, {Type, Date}} ->
      run(pollution:getDailyMean(Type, Date, Monitor));

    {crash} -> 1 / 0;

    _ -> run(Monitor)
  end.

crash() ->
  pollutionServer ! {crash}.

stop() ->
  io:format("stop! ~n"),
  pollutionServer ! stop.