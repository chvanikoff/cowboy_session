-module(cowboy_session).
-author('chvanikoff <chvanikoff@gmail.com>').

%% API
-export([
	start/0,
	on_request/1,
	get/2, get/3,
	set/3,
	expire/1,
	touch/1
]).

-behaviour(application).
-export([start/2, stop/1]).

-behaviour(supervisor).
-export([init/1]).
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

-define(CONFIG(Key), cowboy_session_config:get(Key)).

%% ===================================================================
%% API functions
%% ===================================================================

-spec start() -> ok.
start() ->
	ensure_started([?MODULE]).

-spec on_request(cowboy_req:req()) -> cowboy_req:req().
on_request(Req) ->
	{_Session, Req2} = get_session(Req),
	Req2.

get(Key, Req) ->
	get(Key, undefined, Req).

get(Key, Default, Req) ->
	{Pid, Req2} = get_session(Req),
	Value = cowboy_session_server:get(Pid, Key, Default),
	{Value, Req2}.

set(Key, Value, Req) ->
	{Pid, Req2} = get_session(Req),
	cowboy_session_server:set(Pid, Key, Value),
	{ok, Req2}.

expire(Req) ->
	{Pid, Req2} = get_session(Req),
	cowboy_session_server:stop(Pid),
	Req3 = clear_cookie(Req2),
	{ok, Req3}.

touch(Req) ->
	{_Pid, Req2} = get_session(Req),
	{ok, Req2}.

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
	{ok, Sup} = supervisor:start_link({local, ?MODULE}, ?MODULE, []),
	{ok, Storage} = ?CONFIG(storage),
	supervisor:start_child(Sup, ?CHILD(Storage, worker)),
	{ok, Sup}.

stop(_State) ->
	ok.

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
	Restart_strategy = {one_for_one, 10, 5},
	Children = [
		?CHILD(cowboy_session_server_sup, supervisor),
		?CHILD(cowboy_session_config, worker)
	],
	{ok, {Restart_strategy, Children}}.

%% ===================================================================
%% Internal functions
%% ===================================================================

get_session(Req) ->
	{ok, Cookie_name} = ?CONFIG(cookie_name),
	{SID, Req2} = case cowboy_req:meta(cookie, Req) of
		{undefined, Req3} ->
			cowboy_req:cookie(Cookie_name, Req3);
		Result ->
			Result
	end,
	case SID of
		undefined ->
			create_session(Req2);
		_ ->
			case gproc:lookup_local_name({cowboy_session, SID}) of
				undefined ->
					create_session(Req2);
				Pid ->
					cowboy_session_server:touch(Pid),
					{Pid, Req2}
			end
	end.

clear_cookie(Req) ->
	{ok, Cookie_name} = ?CONFIG(cookie_name),
	{ok, Cookie_options} = ?CONFIG(cookie_options),
	Req2 = cowboy_req:set_meta(cookie, undefined, Req),
	cowboy_req:set_resp_cookie(
		Cookie_name,
		<<"deleted">>,
		[{max_age, 0} | Cookie_options],
		Req2).

create_session(Req) ->
	%% The cookie value cannot contain any of the following characters:
	%%   ,; \t\r\n\013\014
	SID = list_to_binary(uuid:to_string(uuid:v4())),
	{ok, Cookie_name} = ?CONFIG(cookie_name),
	{ok, Cookie_options} = ?CONFIG(cookie_options),
	{ok, Storage} = ?CONFIG(storage),
	{ok, Expire} = ?CONFIG(expire),
	{ok, Pid} = supervisor:start_child(cowboy_session_server_sup, [[
		{sid, SID},
		{storage, Storage},
		{expire, Expire}
	]]),
	Req2 = cowboy_req:set_resp_cookie(Cookie_name, SID, Cookie_options, Req),
	Req3 = cowboy_req:set_meta(cookie, SID, Req2),
	{Pid, Req3}.

ensure_started([]) -> ok;
ensure_started([App | Rest] = Apps) ->
	case application:start(App) of
		ok -> ensure_started(Rest);
		{error, {already_started, App}} -> ensure_started(Rest);
		{error, {not_started, Dependency}} -> ensure_started([Dependency | Apps])
	end.
