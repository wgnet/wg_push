-module(wg_apns_push_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([all/0,
         init_per_suite/1, end_per_suite/1,
         init_per_testcase/2, end_per_testcase/2,
         check_ssl_connection/1,
         apns_push/1, feedback/1
        ]).

all() -> [check_ssl_connection,
          apns_push,
          feedback
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
    Res = ssl:connect("localhost", 2196, ssl_options()),
    ct:pal("ssl connection: ~p", [Res]),
    {ok, Socket} = Res,
    ssl:close(Socket),
    ok.


apns_push(_Config) ->
    Msg = <<"{\"aps\":{\"alert\":\"Hello\",\"badge\":9,\"sound\":\"bingbong.aiff\"},\"param1\":\"value1\",\"param2\":42}">>,
    Token = 16#5382f77562ae26b3ef1ef906cbca99cafe3213d123543dd0f2c6a3e20adf1a61,
    BinToken = <<Token:256/integer>>,

    {Mega, Sec, _Micro} = erlang:now(),
    ExpirationDate = Mega * 1000000 + Sec,

    Res = wg_apns_push:send(Msg, BinToken, {"localhost", 2195}, ssl_options(), ExpirationDate),
    ct:pal("wgn_apns_push:send res:~p", [Res]),
    ok = Res,
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

    Tokens = wg_apns_push:get_feedback({"localhost", 2196}, ssl_options()),
    ct:pal("Tokens:~p", [Tokens]),
    {ok, [{Timestamp2, Token2}, {Timestamp1, Token1}]} = Tokens,
    ok.


%%% Inner functions

ssl_options() ->
    CertFile = "../../test/server.crt",
    KeyFile = "../../test/server.key",
    {ok, _} = file:read_file(CertFile),
    {ok, _} = file:read_file(KeyFile),
    [{certfile, CertFile}, {keyfile, KeyFile}].
