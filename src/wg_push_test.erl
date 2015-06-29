-module(wg_push_test).

-include("wg_push.hrl").
-include_lib("stdlib/include/ms_transform.hrl").

-export([init/0, send_v0/1, send_v1/1, send_v2/1, send_v2/2, send_v2_local/0]).


%% module API

init() ->
    dbg:tracer(),
    %% dbg:p(wg_push_sender, [m]), %% trace all messages to wg_push_sender process
    dbg:p(all, [c]), %% trace function calls

    %% dbg:tp(wg_push_sender, send_messages, cx),
    %% dbg:tpl(wg_push_sender, get_connection, cx),
    %% dbg:tpl(wg_push_sender, send, cx),
    %% dbg:tpl(wg_push_sender, parse_reply, cx),

    dbg:tp(ssl, send, cx),
    dbg:tp(ssl, recv, cx),
    ok.


send_v0(Token) ->
    {Host, Port} = host_port(),
    SSL_Options = ssl_options(),
    Msg = msg(<<"Hello V0">>),
    io:format("Send V0 T:~p~nM:~p~nO:~p~n", [Token, Msg, SSL_Options]),
    case ssl:connect(Host, Port, SSL_Options) of
        {ok, Socket} ->
            ok = ssl:send(Socket, <<0, % protocol
                                    0, 32, Token/binary,
                                    (byte_size(Msg)):16/integer,
                                    Msg/binary>>),
            Res = get_reply(),
            ssl:close(Socket),
            Res;
        {error, Error} -> {error, Error}
    end.


send_v1(Token) ->
    {Host, Port} = host_port(),
    SSL_Options = ssl_options(),
    Msg = msg(<<"Hello V1">>),
    io:format("Send V1 T:~p~nM:~p~nO:~p~n", [Token, Msg, SSL_Options]),
    case ssl:connect(Host, Port, SSL_Options) of
        {ok, Socket} ->
            ok = ssl:send(Socket, <<1, % protocol
                                    0,0,0,1, % identifier
                                    0,0,0,1, % expiration date
                                    0, 32, Token/binary,
                                    (byte_size(Msg)):16/integer,
                                    Msg/binary>>),
            Res = get_reply(),
            ssl:close(Socket),
            Res;
        {error, Error} -> {error, Error}
    end.


send_v2(Token) -> send_v2(Token, <<"Hello from V2">>).

send_v2(Token, Text) ->
    SSL_Options = #wg_push_ssl_options{certfile = "../../tmp/keys/cert_1.pem",
                                       keyfile = "../../tmp/keys/pkey.pem"},
    Msg = #wg_push_item{id = 1,
                        device_token = Token,
                        payload = msg(Text),
                        expiration_date = 0
                       },
    io:format("Send V2 T:~p~nM:~p~nO:~p~n", [Token, Msg, SSL_Options]),
    wg_push_sender:send_message(Msg, SSL_Options).


send_v2_local() ->
    SSL_Options = #wg_push_ssl_options{certfile = "test/server.crt",
                                       keyfile = "test/server.key"},
    Token = <<1,1,1,1, 1,1,1,1,
              2,2,2,2, 2,2,2,2,
              3,3,3,3, 3,3,3,3,
              4,4,4,4, 4,4,4,4>>,
    Msg1 = #wg_push_item{id = 20,
                         device_token = Token,
                         payload = msg(<<"Hello">>),
                         expiration_date = 0
                        },
    Msg2 = #wg_push_item{id = 2,
                         device_token = Token,
                         payload = msg(<<"Hello again">>),
                         expiration_date = 0
                        },
    wg_push_sender:set_apns_host_port("localhost", 2195),
    wg_push_sender:send_messages([Msg1, Msg2], SSL_Options).


%%% inner functions

host_port() -> {"gateway.push.apple.com", 2195}.


ssl_options() ->
    [{certfile, "../../tmp/keys/cert_1.pem"},
     {keyfile, "../../tmp/keys/pkey.pem"},
     {versions, ['tlsv1.1']},
     {active, true},
     binary].


msg(Text) -> <<"{\"aps\":{\"alert\":\"", Text/binary, "\"}}">>.


get_reply() ->
    receive
        {ssl, _, Bin} -> {reply, Bin};
        {ssl_closed, _} -> ssl_closed
    after
        1000 -> ssl_timeout
    end.
