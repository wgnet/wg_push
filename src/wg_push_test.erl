-module(wg_push_test).

-include("wg_push.hrl").
-include_lib("stdlib/include/ms_transform.hrl").

-export([send_message1/0, send_message2/0, send_message/1,
         enable_tracing/0, disable_tracing/0]).


send_message1() ->
    SSL_Options = #wg_push_ssl_options{certfile = "./test/server.crt",
                                       keyfile = "./test/server.key"},
    send_message(SSL_Options).


send_message2() ->
    SSL_Options = #wg_push_ssl_options{certfile = "./test/server2.crt",
                                       keyfile = "./test/server2.key"},
    send_message(SSL_Options).


send_message(SSL_Options) ->
    DeviceToken = <<1,1,1,1,1,1,1,1,
                    2,2,2,2,2,2,2,2,
                    3,3,3,3,3,3,3,3,
                    4,4,4,4,4,4,4,4>>,
    Payload = <<"{\"alert\":\"hello\"}">>,
    Message = #wg_push_item{id = 1,
                            device_token = DeviceToken,
                            payload = Payload,
                            expiration_date = 123
                           },
    wg_push_sender:send_message(Message, SSL_Options).


enable_tracing() ->
    dbg:tracer(),
    %% dbg:p(wg_push_sender, [m]), %% trace all messages to wg_push_sender process
    dbg:p(all, [c]), %% trace function calls

    dbg:tp(wg_push_sender, send_message, cx),
    dbg:tp(wg_push_sender, handle_call, cx),
    dbg:tp(wg_push_sender, handle_cast, cx),
    dbg:tpl(wg_push_sender, create_connection, cx),
    dbg:tpl(wg_push_sender, send, cx),
    ok.


disable_tracing() ->
    dbg:stop_clear(),
    ok.
