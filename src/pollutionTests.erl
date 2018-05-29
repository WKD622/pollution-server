%%%-------------------------------------------------------------------
%%% @author jakub
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 19. Apr 2018 01:04
%%%-------------------------------------------------------------------
-module(pollutionTests).
-author("jakub").

-include_lib("eunit/include/eunit.hrl").

%% API
-export([]).

createMonitor_test() ->
  ?assertEqual('pollution':createMonitor(), []).

addStationError_test() ->
  A = 'pollution':createMonitor(),
  ?assertNotMatch({error, _}, 'pollution':addStationToMonitor("a", {2, 3}, A)).

addStation_test() ->
  A = 'pollution':createMonitor(),
  ?assertEqual('pollution':addStationToMonitor("a", {2, 3}, A), [{station, "a", {2, 3}, []}]).

addValue_test() ->
  A = 'pollution':createMonitor(),
  B = 'pollution':addStationToMonitor("a", {2, 3}, A),
  ?assertEqual('pollution':addValue("a", "2000", "PM", 1, B), [{station, "a", {2, 3}, [{measure, "2000", "PM", 1}]}]).

remove_test() ->
  A = 'pollution':createMonitor(),
  B = 'pollution':addStationToMonitor("a", {2, 3}, A),
  C = 'pollution':addValue("a", "2000", "PM", 1, B),
  ?assertEqual('pollution':removeValue("a", "2000", "PM", C), [{station, "a", {2, 3}, []}]).
