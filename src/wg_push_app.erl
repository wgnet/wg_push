-module(wg_push_app).
-behaviour(application).

-export([start/0, start/2, stop/1]).


-spec(start() -> ok).
start() ->
    application:start(crypto),
    application:start(asn1),
    application:start(public_key),
    application:start(ssl),
    ok = application:start(wg_push),
    ok.


-spec(start(term(), term()) -> {ok, pid()}).
start(_StartType, _StartArgs) ->
    wg_push_sup:start_link().


-spec(stop(term()) -> ok).
stop(_State) ->
    ok.
