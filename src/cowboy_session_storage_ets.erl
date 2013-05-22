-module(cowboy_session_storage_ets).
-author('chvanikoff <chvanikoff@gmail.com>').

%% Cowboy_session_storage behaviour
-behaviour(cowboy_session_storage).
-export([
	start_link/0,
	new/1,
	set/3,
	get/3,
	delete/1,
	stop/1
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

-define(TABLE, cowboy_session).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

new(SID) ->
	gen_server:cast(?MODULE, {new, SID}).

set(SID, Key, Value) ->
	gen_server:cast(?MODULE, {set, SID, Key, Value}).

get(SID, Key, Default) ->
	gen_server:call(?MODULE, {get, SID, Key, Default}).

delete(SID) ->
	gen_server:cast(?MODULE, {delete, SID}).

stop(New_storage) ->
	gen_server:cast(?MODULE, {stop, New_storage}).


%%%===================================================================
%%% Gen_server callbacks
%%%===================================================================

init([]) ->
	?TABLE = ets:new(?TABLE, [public, named_table]),
	{ok, undefined}.


handle_call({get, SID, Key, Default}, _From, State) ->
	Reply = case ets:lookup(?TABLE, SID) of
		[] -> Default;
		[{SID, Data}] ->
			case lists:keyfind(Key, 1, Data) of
				{_, Value} -> Value;
				_ -> Default
			end
	end,
	{reply, Reply, State};

handle_call(_, _, State) -> {reply, ignored, State}.


handle_cast({new, SID}, State) ->
	true = ets:insert_new(?TABLE, {SID, []}),
	{noreply, State};

handle_cast({set, SID, Key, Value}, State) ->
	[{SID, Data}] = ets:lookup(?TABLE, SID),
	Data2 = lists:keystore(Key, 1, Data, {Key, Value}),
	ets:insert(?TABLE, {SID, Data2}),
	{noreply, State};

handle_cast({delete, SID}, State) ->
	ets:delete(?TABLE, SID),
	{noreply, State};

handle_cast({stop, _New_storage}, State) ->
	{stop, normal, State};

handle_cast(_, State) -> {noreply, State}.


handle_info(_, State) -> {noreply, State}.


terminate(_Reason, _State) ->
	ets:delete(?TABLE),
	ok.


code_change(_OldVsn, State, _Extra) ->
	{ok, State}.
