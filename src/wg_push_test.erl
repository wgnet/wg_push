-module(wg_push_test).

-include("wg_push.hrl").
-include_lib("stdlib/include/ms_transform.hrl").

-export([init/0, send1/0, send2/0, send_message/1, send_v1/0]).

-define(TOKEN, <<1,1,1,1, 1,1,1,1,
                 2,2,2,2, 2,2,2,2,
                 3,3,3,3, 3,3,3,3,
                 4,4,4,4, 4,4,4,4>>).

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


send1() ->
    SSL_Options = #wg_push_ssl_options{certfile = "../../tmp/pc.pem",
                                       keyfile = "../../tmp/pk.pem"},
    send_message(SSL_Options).


send2() ->
    SSL_Options = #wg_push_ssl_options{certfile = "./test/server.crt",
                                       keyfile = "./test/server.key"},
    send_message(SSL_Options).


send_message(SSL_Options) ->
    Payload = <<"{\"aps\":{\"alert\":\"Hello\"}}">>,
    Message = #wg_push_item{id = 1,
                            device_token = ?TOKEN,
                            payload = Payload,
                            expiration_date = 0
                           },
    wg_push_sender:send_message(Message, SSL_Options).


send_v1() ->
    Host = "gateway.push.apple.com",
    Port = 2195,
    SSL_Options = [{certfile, "../../tmp/pc.pem"},
                   {keyfile, "../../tmp/pk.pem"},
                   {active, true},
                   binary],
    case ssl:connect(Host, Port, SSL_Options) of
        {ok, Socket} ->
            Msg = <<"{\"aps\":{\"alert\":\"Hello\"}}">>,
            MSize = byte_size(Msg),
            DeviceToken = ?TOKEN,
            Payload = <<1, % protocol
                        0,0,0,1, % identifier
                        0,0,0,1, % expiration date
                        0, 32, DeviceToken/binary,
                        MSize:16/integer, Msg/binary>>,
            ok = ssl:send(Socket, Payload),
            Res = receive
                      {ssl, _, Bin} -> {reply, Bin};
                      {ssl_closed, _} -> ssl_closed
                  after 1000 -> ssl_timeout
                  end,
            ssl:close(Socket),
            Res;
        {error, Error} -> {error, Error}
    end.
