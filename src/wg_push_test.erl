-module(wg_push_test).

-include("wg_push.hrl").
-include_lib("stdlib/include/ms_transform.hrl").

-export([init/0, send_v0/0, send_v1/0, send_v2/0]).

-define(TOKEN,
<<89,245,110,5,  76,255,9,240,
  101,244,6,124, 126,54,53,119,
  179,23,162,68, 241,115,252,208,
  195,19,42,255, 26,106,19,26>>).

        %% <<1,1,1,1, 1,1,1,1,
        %%   2,2,2,2, 2,2,2,2,
        %%   3,3,3,3, 3,3,3,3,
        %%   4,4,4,4, 4,4,4,4>>).


%% module API

init() ->
    dbg:tracer(),
    %% dbg:p(wg_push_sender, [m]), %% trace all messages to wg_push_sender process
    dbg:p(all, [c]), %% trace function calls

    dbg:tp(wg_push_sender, send_message, cx),
    %% dbg:tp(wg_push_sender, handle_call, cx),
    %% dbg:tp(wg_push_sender, handle_cast, cx),
    dbg:tpl(wg_push_sender, get_connection, cx),
    dbg:tpl(wg_push_sender, send, cx),
    dbg:tpl(wg_push_sender, parse_reply, cx),

    dbg:tp(wg_push_pack, pack_items, 1, cx),

    %% dbg:tp(ssl, connect, cx),
    ok.


send_v0() ->
    {Host, Port} = host_port(),
    SSL_Options = ssl_options(),
    Msg = msg(),
    case ssl:connect(Host, Port, SSL_Options) of
        {ok, Socket} ->
            ok = ssl:send(Socket, <<0, % protocol
                                    0, 32, ?TOKEN/binary,
                                    (byte_size(Msg)):16/integer,
                                    Msg/binary>>),
            Res = get_reply(),
            ssl:close(Socket),
            Res;
        {error, Error} -> {error, Error}
    end.


send_v1() ->
    {Host, Port} = host_port(),
    SSL_Options = ssl_options(),
    Msg = msg(),
    case ssl:connect(Host, Port, SSL_Options) of
        {ok, Socket} ->
            ok = ssl:send(Socket, <<1, % protocol
                                    0,0,0,1, % identifier
                                    0,0,0,1, % expiration date
                                    0, 32, ?TOKEN/binary,
                                    (byte_size(Msg)):16/integer,
                                    Msg/binary>>),
            Res = get_reply(),
            ssl:close(Socket),
            Res;
        {error, Error} -> {error, Error}
    end.


send_v2() ->
    SSL_Options = #wg_push_ssl_options{certfile = "../../tmp/pc.pem",
                                       keyfile = "../../tmp/pk.pem"},
    Message = #wg_push_item{id = 1,
                            device_token = ?TOKEN,
                            payload = msg(),
                            expiration_date = 0
                           },
    wg_push_sender:send_message(Message, SSL_Options).


%%% inner functions

host_port() -> {"gateway.push.apple.com", 2195}.


ssl_options() ->
    [{certfile, "../../tmp/pc.pem"},
     {keyfile, "../../tmp/pk.pem"},
     {active, true},
     binary].


msg() -> <<"{\"aps\":{\"alert\":\"Hello\"}}">>.


get_reply() ->
    receive
        {ssl, _, Bin} -> {reply, Bin};
        {ssl_closed, _} -> ssl_closed
    after
        1000 -> ssl_timeout
    end.
