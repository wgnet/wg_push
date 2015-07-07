-module(wg_apns_push_SUITE).

-include_lib("common_test/include/ct.hrl").
-include("wg_push.hrl").

-export([all/0,
         init_per_suite/1, end_per_suite/1,
         init_per_testcase/2, end_per_testcase/2,
         check_ssl_connection/1,
         apns_push/1, feedback/1,
         encode_aps/1, encode_alert/1
        ]).

all() -> [check_ssl_connection,
          apns_push,
          feedback,
          encode_aps,
          encode_alert
         ].


%%% Setup

init_per_suite(Config) ->
    Res = os:cmd("cd ../../test; make start-emulator"),
    ct:pal("start apns emulators ~p", [Res]),
    timer:sleep(500), % wait for emulator to init
    ssl:start(),
    Config.


end_per_suite(Config) ->
    Res = os:cmd("cd ../../test; make stop-emulator"),
    ct:pal("stop apns emulators ~p", [Res]),
    ssl:stop(),
    Config.


init_per_testcase(_, Config) ->
    Config.


end_per_testcase(_, Config) ->
    Config.


%%% Tests

check_ssl_connection(_Config) ->
    Options = wg_push_pack:build_ssl_options(ssl_options()),
    Res = ssl:connect("localhost", 2195, Options),
    ct:pal("ssl connection: ~p", [Res]),
    {ok, Socket} = Res,
    ssl:close(Socket),
    ok.


apns_push(_Config) ->
    SSL_Options = ssl_options(),
    Token = <<1,1,1,1, 1,1,1,1, 2,2,2,2, 2,2,2,2,
              3,3,3,3, 3,3,3,3, 4,4,4,4, 4,4,4,4>>,
    Payload = <<"{\"aps\":{\"alert\":\"Hello\"}}">>,
    Msg1 = #wg_push_item{id = 20,
                         device_token = Token,
                         payload = Payload,
                         expiration_date = 123
                        },
    Msg2 = #wg_push_item{id = 2,
                         device_token = Token,
                         payload = Payload,
                         expiration_date = 456
                        },
    wg_push_app:start(),

    wg_push_sender:set_apns_host_port("localhost", 99999),
    {error, no_connection, _} = wg_push_sender:send_messages([Msg1, Msg2], SSL_Options),

    wg_push_sender:set_apns_host_port("localhost", 2195),
    ok = wg_push_sender:send_messages([Msg1, Msg2], SSL_Options),
    ok = wg_push_sender:send_message(Msg1, SSL_Options),

    {error, pack, invalid_device_token} = wg_push_sender:send_message(
                                            Msg1#wg_push_item{device_token = <<1,2,3>>}, SSL_Options),

    ok = wg_push_sender:send_message(Msg1#wg_push_item{id = 0}, SSL_Options),
    {error, reply, processing_error} = wg_push_sender:send_message(Msg1#wg_push_item{id = 1}, SSL_Options),
    {error, reply, missing_device_token} = wg_push_sender:send_message(Msg1#wg_push_item{id = 2}, SSL_Options),
    {error, reply, missing_topic} = wg_push_sender:send_message(Msg1#wg_push_item{id = 3}, SSL_Options),
    {error, reply, missing_payload} = wg_push_sender:send_message(Msg1#wg_push_item{id = 4}, SSL_Options),
    {error, reply, invalid_token_size} = wg_push_sender:send_message(Msg1#wg_push_item{id = 5}, SSL_Options),
    {error, reply, invalid_topic_size} = wg_push_sender:send_message(Msg1#wg_push_item{id = 6}, SSL_Options),
    {error, reply, invalid_payload_size} = wg_push_sender:send_message(Msg1#wg_push_item{id = 7}, SSL_Options),
    {error, reply, invalid_token} = wg_push_sender:send_message(Msg1#wg_push_item{id = 8}, SSL_Options),
    {error, reply, unknown_reply} = wg_push_sender:send_message(Msg1#wg_push_item{id = 9}, SSL_Options),
    {error, reply, unknown_error} = wg_push_sender:send_message(Msg1#wg_push_item{id = 255}, SSL_Options),

    ok.


feedback(_Config) ->
    Timestamp1 = 1422421871,
    Token1 = <<111, 111, 111, 111, 111, 111, 111, 111,
               111, 111, 111, 111, 111, 111, 111, 111,
               111, 111, 111, 111, 111, 111, 111, 111,
               111, 111, 111, 111, 111, 111, 111, 111>>,
    Timestamp2 = 1422450398,
    Token2 = <<222, 222, 222, 222, 222, 222, 222, 222,
               222, 222, 222, 222, 222, 222, 222, 222,
               222, 222, 222, 222, 222, 222, 222, 222,
               222, 222, 222, 222, 222, 222, 222, 222>>,

    Tokens = wg_push_feedback:get_feedback({"localhost", 2196}, ssl_options()),
    ct:pal("Tokens:~p", [Tokens]),
    {ok, [{Timestamp2, Token2}, {Timestamp1, Token1}]} = Tokens,
    ok.

encode_aps(_Config) ->
    P1 = #wg_push_aps{
        alert = #wg_push_alert{title = <<"T1">>, body = <<"B1">>},
        badge = 1,
        sound = <<"q">>
    },
    R1 = <<"{\"aps\":{\"sound\":\"q\",\"badge\":1,\"alert\":{\"body\":\"B1\",\"title\":\"T1\"}}}">>,
    R1 = wg_push_pack:encode_aps(P1),

    P2 = #wg_push_aps{
        alert = <<"A">>,
        badge = 1,
        sound = <<"q">>
    },
    R2 = <<"{\"aps\":{\"sound\":\"q\",\"badge\":1,\"alert\":\"A\"}}">>,
    R2 = wg_push_pack:encode_aps(P2),

    P3 = #wg_push_aps{
        alert = <<"A">>,
        badge = 1,
        sound = <<"q">>,
        data = [{q, <<"b">>}, {w, 17}]
    },
    R3 = <<"{\"aps\":{\"sound\":\"q\",\"badge\":1,\"alert\":\"A\"},\"q\":\"b\",\"w\":17}">>,
    R3 = wg_push_pack:encode_aps(P3),

    P4 = #wg_push_aps{
        alert = #wg_push_alert{title = <<"T1">>, body = <<"B1">>, title_loc_args = [<<"a">>, <<"b">>, <<"c">>]},
        badge = 1,
        sound = <<"q">>
    },
    R4 = <<"{\"aps\":{\"sound\":\"q\",\"badge\":1,\"alert\":{\"title-loc-args\":[\"a\",\"b\",\"c\"],\"body\":\"B1\",\"title\":\"T1\"}}}">>,
    R4 = wg_push_pack:encode_aps(P4),

    ok.

encode_alert(_Config) ->
    A1 = #wg_push_alert{title = <<"T1">>, body = <<"B1">>},
    E1 = [{body, <<"B1">>}, {title, <<"T1">>}],
    {R1} = wg_push_pack:encode_alert(A1),
    E1 = lists:sort(R1),

    A2 = #wg_push_alert{title = <<"T1">>, body = <<"B1">>, loc_args = [<<"1">>, <<"2">>]},
    E2 = [{body, <<"B1">>}, {'loc-args', [<<"1">>, <<"2">>]}, {title, <<"T1">>}],
    {R2} = wg_push_pack:encode_alert(A2),
    E2 = lists:sort(R2),

    ok.


%%% Inner functions

ssl_options() ->
    CertFile = "../../test/server.crt",
    KeyFile = "../../test/server.key",
    {ok, _} = file:read_file(CertFile),
    {ok, _} = file:read_file(KeyFile),
    #wg_push_ssl_options{certfile = CertFile,
                         keyfile = KeyFile}.
