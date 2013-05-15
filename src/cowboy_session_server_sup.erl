-module(cowboy_session_server_sup).
-author('chvanikoff <chvanikoff@gmail.com>').

%% API
-export([start_link/0]).

%% Supervisor behaviour
-behaviour(supervisor).
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================
start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================
init([]) ->
	Restart_strategy = {simple_one_for_one, 0, 1},
	Children = [
		{cowboy_session_server,
			{cowboy_session_server, start_link, []},
			transient, 5000, worker, [cowboy_session_server]}
	],
	{ok, {Restart_strategy, Children}}.
