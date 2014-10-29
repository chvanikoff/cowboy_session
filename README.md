# Cowboy session

## Usage:
First you need to get dependencies (gproc and erlang-uuid) and compile the code:
```bash
make
```
or
```bash
make deps
make compile
```

Then you need the cowboy_session application to be started:
```erlang
cowboy_session:start().
```
or manually
```erlang
application:start(gproc),
application:start(uuid),
application:start(cowboy_session).
```

If you want session to be started for each request then use function `cowboy_session:on_request/1` as Cowboy onrequest callback:
```erlang
cowboy:start_http(http_listener, Nba, [{port, Port}], [
		{env, [
			{dispatch, Dispatch}
		]},
		{onrequest, fun cowboy_session:on_request/1} %% < setting on_request callback
	]).
```
otherwise first call to `cowboy_session:set/3` or `cowboy_session:get/2/3` will initialize session

Set some key to some value:
```erlang
{ok, Req2} = cowboy_session:set(Key, Value, Req)
```

Get previously setted key:
```erlang
{Value, Req3} = cowboy_session:get(Key, Req2)
```

Get some key using default value to return if key will not be found:
```erlang
{Value, Req3} = cowboy_session:get(Key, Default, Req2)
```

You can configure cowboy_session with following params:
- cookie_name - cookie name. Default: `<<session>>`
- cookie_options - list of cookie options. Default: `[{path, <<"/">>}]`
- expire - session expiration time, in seconds. Default: `1440`

To change any of them use `cowboy_session_config:set/1/2`:
```erlang
ok = cowboy_session_config:set(cookie_options, [{path, <<"/">>}, {domain, <<".site.com">>}]),
ok = cowboy_session_config:set([
	{cookie_name, <<"new_cookie_name">>},
	{expire, 86400}
])
```

Also you can use your own storage for sessions. Just implement it (see behaviour cowboy_session_storage or cowboy_session_storage_ets as an example) and run `cowboy_session_config:update_storage(New_storage_name)` - this will call `stop/0` callback for currently running storage and will initialize the new one. Default (and currently the only one) session storage is cowboy_session_storage_ets.
