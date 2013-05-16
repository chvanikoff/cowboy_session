-module(cowboy_session_config).
-author('chvanikoff <chvanikoff@gmail.com>').

%% API
-export([
	start_link/0,
	set/1, set/2,
	get/1, get/2,
	update_storage/1
]).

%% Gen_server behaviour
-behaviour(gen_server).
-export([
	init/1,
	handle_call/3,
	handle_cast/2,
	handle_info/2,
	terminate/2,
	code_change/3
]).

-define(DEFAULT, [
	{cookie_name, <<"session">>},
	{cookie_options, [{path, <<"/">>}]},
	{expire, 1440},
	{storage, cowboy_session_storage_ets}
]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

set(Config) ->
	lists:foreach(fun({Key, Value}) -> ?MODULE:set(Key, Value) end, Config).

set(Key, Value) ->
	gen_server:cast(?MODULE, {set, Key, Value}).

update_storage(Value) ->
	case lists:keyfind(storage, 1, ?DEFAULT) of
		{_, Value} -> ok;
		_ -> gen_server:cast(?MODULE, {update_storage, Value})
	end.

get(Key) ->
	get(Key, undefined).

get(Key, Default) ->
	gen_server:call(?MODULE, {get, Key, Default}).

%% ===================================================================
%% Gen_server callbacks
%% ===================================================================

init([]) ->
	{ok, ?DEFAULT}.

handle_call({get, Key, Default}, _From, State) ->
	Result = case lists:keyfind(Key, 1, State) of
		{_, Value} -> Value;
		_ -> Default
	end,
	{reply, Result, State};
handle_call(_Request, _From, State) ->
	{reply, ignored, State}.

handle_cast({set, Key, Value}, State) ->
	{noreply, lists:keystore(Key, 1, State, {Key, Value})};
handle_cast({update_storage, Value}, State) ->
	{ok, Pid} = supervisor:start_child(cowboy_session, {Value, {Value, start_link, []}, permanent, 5000, worker, [Value]}),
	{_, Storage} = lists:keyfind(storage, 1, State),
	Storage:stop(Pid),
	supervisor:terminate_child(cowboy_session, self()),
	supervisor:delete_child(cowboy_session, ?MODULE),
	{noreply, lists:keystore(storage, 1, State, {storage, Value})};
handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.
