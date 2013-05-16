-module(cowboy_session_storage).
-author('chvanikoff <chvanikoff@gmail.com>').

-type error() :: {already_started, pid()} | term().

-callback start_link() -> {ok, pid()} | ignore | {error, error()}.

-callback new(SID) -> ok when
	SID :: bitstring().

-callback set(SID, Key, Value) -> ok when
	SID :: bitstring(),
	Key :: term(),
	Value :: term().

-callback get(SID, Key, Default) -> term() when
	SID :: bitstring(),
	Key :: term(),
	Default :: term().

-callback delete(SID) -> ok when
	SID :: bitstring().

-callback stop(New_storage) -> ok when
	New_storage :: pid().
