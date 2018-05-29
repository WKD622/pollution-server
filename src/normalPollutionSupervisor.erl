%%%-------------------------------------------------------------------
%%% @author jakub
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. May 2018 13:30
%%%-------------------------------------------------------------------
-module(normalPollutionSupervisor).
-author("jakub").

%% API
-export([start/0, init/0]).

start() ->
  register(supervisor, spawn(?MODULE, init, [])).

init() ->
  normalPollutionServer:start(),
  process_flag(trap_exit, true),
  loop().

loop() ->
  receive
    {'EXIT', _, _} -> io:format("crashed"),
      normalPollutionServer:start(),
      loop()
  end.
