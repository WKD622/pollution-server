%%%-------------------------------------------------------------------
%%% @author kuba
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 22. May 2018 20:23
%%%-------------------------------------------------------------------
-module(otpPollutionSupervisor).
-behavior(supervisor).
-author("kuba").

%%-----------------------------------------------------------------------------
%% API Function Exports
%%-----------------------------------------------------------------------------

-export([start_link/0, init/1]).

%% ---------------------------------------------------------------------------
%% API Function Definitions
%% ---------------------------------------------------------------------------

start_link() ->
  otpPollutionServer:start_link().

init(_args) ->
  o:format("Supervisor started~n"),
  {ok,                      % ok, supervisor here's what we want you to do
    {
      {                     % Global supervisor options
        one_for_all,        % - use the one-for-one restart strategy
        2,                  % - and allow a maximum of 2 restarts
        3                   % - per hour for each child process
      },
      [                     % The list of child processes you should supervise
        {                   % We only have one
          otpPollutionServer,     % - Register it under the name hello_server
          {                 % - Here's how to find and start this child's code
            otpPollutionServer,   %   * the module is called hello_server
            start,          %   * the function to invoke is called start_link
            []              %   * and here's the list of default parameters to use
          },
          permanent,        % - child should run permantenly, restart on crash
          brutal_kill,
          worker,           % - FYI, this child is a worker, not a supervisor
          [otpPollutionServer]    % - these are the modules the process uses
        }
      ]
    }
  }.