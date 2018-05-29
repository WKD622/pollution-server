%%%-------------------------------------------------------------------
%%% @author Jakub Ziarko
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. May 2018 18:51
%%%-------------------------------------------------------------------

-module(otpPollutionServer).
-behavior(gen_server).
-define(POLLUTION_SERVER, ?MODULE).
-author("kuba").

%%-----------------------------------------------------------------------------
%% API Function Exports
%%-----------------------------------------------------------------------------

-export([
  start_link/0,
  stop/0,
  addStation/2,
  addValue/4,
  removeValue/3,
  getOneValue/3,
  crash/0, getDailyMean/2, getStationMean/2]).

%% ---------------------------------------------------------------------------
%% gen_server Function Exports
%% ---------------------------------------------------------------------------

-export([
  init/1,
  handle_call/3,
  handle_cast/2,
  code_change/3,
  getMonitor/0, terminate/2, handle_info/2]).

%% ---------------------------------------------------------------------------
%% API Function Definitions
%% ---------------------------------------------------------------------------

start_link() ->
  gen_server:start({local, ?POLLUTION_SERVER}, ?MODULE, [], []).

stop() ->
  gen_server:cast(?POLLUTION_SERVER, stop).

addStation(Name, Coordinates) ->
  gen_server:cast(?POLLUTION_SERVER, {addStation, Name, Coordinates}).

addValue(Station, Date, Type, Value) ->
  gen_server:cast(?POLLUTION_SERVER, {addValue, Station, Date, Type, Value}).

removeValue(Station, Date, Type) ->
  gen_server:cast(?POLLUTION_SERVER, {removeValue, Station, Date, Type}).

getOneValue(Station, Date, Type) ->
  gen_server:call(?POLLUTION_SERVER, {getOneValue, Station, Date, Type}).

getDailyMean(Type, {Day, Month, Year}) ->
  gen_server:call(?POLLUTION_SERVER, {getDailyMean, Type, {Day, Month, Year}}).

getStationMean(Station, Type) ->
  gen_server:call(?POLLUTION_SERVER, {getStationMean, Station, Type}).

getMonitor() ->
  gen_server:call(?POLLUTION_SERVER, get_state).

crash() ->
  gen_server:cast(?POLLUTION_SERVER, crash).

%% ---------------------------------------------------------------------------
%% gen_server Function Definitions
%% ---------------------------------------------------------------------------

init(_args) ->
  {ok, pollution:createMonitor()}.

handle_cast(crash, Monitor) ->
  io:format("crashing~n"),
  1 / 0,
  {noreply, Monitor};

handle_cast({addStation, Name, Coordinates}, Monitor) ->
  NewMonitor = controlResult(Monitor, pollution:addStation(Name, Coordinates, Monitor)),
  {noreply, NewMonitor};

handle_cast({addValue, Station, Date, Type, Value}, Monitor) ->
  NewMonitor = controlResult(Monitor, pollution:addValue(Station, Date, Type, Value, Monitor)),
  {noreply, NewMonitor};

handle_cast({removeValue, Station, Date, Type}, Monitor) ->
  NewMonitor = controlResult(Monitor, pollution:removeValue(Station, Date, Type, Monitor)),
  {noreply, NewMonitor}.

handle_call(get_state, _From, Monitor) ->
  {reply, Monitor, Monitor};

handle_call({removeValue, Station, Date, Type}, _From, Monitor) ->
  case pollution:getOneValue(Station, Date, Type, Monitor) of
    {error, Cause} -> {reply, Cause, Monitor};
    value -> {reply, value, Monitor}
  end;

handle_call({getDailyMean, Type, {Day, Month, Year}, Monitor}, _From, Monitor) ->
  case pollution:getDailyMean(Type, {Day, Month, Year}, Monitor) of
    {error, Cause} -> {reply, Cause, Monitor};
    value -> {reply, value, Monitor}
  end;

handle_call({getStationMean, Station, Type}, _From, Monitor) ->
  case pollution:getStationMean(Station, Type, Monitor) of
    {error, Cause} -> {reply, Cause, Monitor};
    value -> {reply, value, Monitor}
  end.

handle_info(Info, Monitor) ->                % handle_info deals with out-of-band msgs, ie
  error_logger:info_msg("~p~n", [Info]),     % msgs that weren't sent via cast
  {noreply, Monitor}.                        % or call. Here we simply log such messages.

terminate(_Reason, _Monitor) ->              % terminate is invoked by the gen_server
  error_logger:info_msg("terminating~n"),    % container on shutdown.
  ok.                                        % we log it and acknowledge with ok.

code_change(_OldVsn, Monitor, _Extra) ->     % called during release up/down-
  {ok, Monitor}.

controlResult(Monitor, {error, Description}) ->
  io:format("error: ~s~n", [Description]),
  Monitor;

controlResult(_, Monitor) ->
  io:format("~w~n", [Monitor]),
  Monitor.


%%handle_call({addStation, Name, Coordinates}, _From, Monitor) ->
%%  case pollution:addStation(Name, Coordinates, Monitor) of
%%    {error, Cause} -> {reply, Cause, Monitor};
%%    [NewMonitor] -> {reply, "station added successfully", NewMonitor}
%%  end;

%%handle_call({addValue, Station, Date, Type, Value}, _From, Monitor) ->
%%  case pollution:addValue(Station, Date, Type, Value, Monitor) of
%%    {error, Cause} -> {reply, Cause, Monitor};
%%    [NewMonitor] -> {reply, "value added successfully", NewMonitor}
%%  end;

%%handle_call({removeValue, Station, Date, Type}, _From, Monitor) ->
%%  case pollution:removeValue(Station, Date, Type, Monitor) of
%%    {error, Cause} -> {reply, Cause, Monitor};
%%    [NewMonitor] -> {reply, "removed successfully", NewMonitor}
%%  end;