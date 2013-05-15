-module(cowboy_session_server).
-author('chvanikoff <chvanikoff@gmail.com>').

%% API
-export([
	start_link/1,
	touch/1,
	stop/1,
	get/3,
	set/3,
	sid/1
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

-record(state, {
	sid,
	expire,
	expire_tref,
	storage
}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(Config) ->
	gen_server:start_link(?MODULE, Config, []).

touch(Pid) ->
	gen_server:cast(Pid, touch).

set(Pid, Key, Value) ->
	gen_server:cast(Pid, {set, Key, Value}).

get(Pid, Key, Default) ->
	gen_server:call(Pid, {get, Key, Default}).

sid(Pid) ->
	gen_server:call(Pid, sid).

stop(Pid) ->
	gen_server:cast(Pid, stop).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================


init(Config) ->
	{_, SID} = lists:keyfind(sid, 1, Config),
	{_, Expire} = lists:keyfind(expire, 1, Config),
	{_, Storage} = lists:keyfind(storage, 1, Config),
	gproc:add_local_name({cowboy_session, SID}),
	{ok, Expire_TRef} = timer:exit_after(Expire * 1000, expire),
	{ok, #state{
		sid = SID,
		expire = Expire,
		expire_tref = Expire_TRef,
		storage = Storage
	}}.


handle_call({get, Key, Default}, _From, #state{sid = SID, storage = Storage} = State) ->
	Value = Storage:get(SID, Key, Default),
	{reply, Value, State};

handle_call(sid, _From, #state{sid = SID} = State) ->
	{reply, SID, State};

handle_call(_, _, State) -> {reply, ignored, State}.


handle_cast({set, Key, Value}, #state{sid = SID, storage = Storage} = State) ->
	ok = Storage:set(SID, Key, Value),
	{noreply, State};

handle_cast(touch, #state{expire = Expire, expire_tref = Expire_TRef} = State) ->
	{ok, cancel} = timer:cancel(Expire_TRef),
	{ok, New_TRef} = timer:exit_after(Expire * 1000, expire),
	{noreply, State#state{expire_tref = New_TRef}};

handle_cast(stop, #state{expire_tref = Expire_TRef} = State) ->
	timer:cancel(Expire_TRef),
	{stop, stopped, State#state{expire_tref = nil}};

handle_cast(_, State) -> {noreply, State}.


handle_info(_, State) -> {noreply, State}.


terminate(_Reason, #state{storage = Storage, sid = SID}) ->
	Storage:delete(SID),
	gproc:goodbye(),
	ok.


code_change(_OldVsn, State, _Extra) ->
	{ok, State}.
