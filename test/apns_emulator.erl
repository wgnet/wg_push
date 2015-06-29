-module(apns_emulator).

-export([start/0, client_session/1]).


start() ->
    spawn(fun start_server/0),
    spawn(fun start_feedback/0),
    ok.


start_server() ->
    ssl:start(),
    Port = 2195,
    SSL_Options = [{certfile, "server.crt"},
                   {keyfile, "server.key"},
                   {reuseaddr, true},
                   {active, false},
                   binary],
    io:format("start apns_emulator ~p at port:~p~n", [self(), Port]),
    {ok, ListenSocket} = ssl:listen(Port, SSL_Options),
    spawn(?MODULE, client_session, [ListenSocket]),
    timer:sleep(infinity).


client_session(ListenSocket) ->
    io:format("~p client_session~n", [self()]),
    {ok, Socket} = ssl:transport_accept(ListenSocket),
    ok = ssl:ssl_accept(Socket),
    io:format("~p client accepted~n", [self()]),
    spawn(?MODULE, client_session, [ListenSocket]),
    read_data(Socket).


read_data(Socket) ->
    {ok, <<2, PacketSize:32/integer>>} = ssl:recv(Socket, 5),
    {ok, Data} = ssl:recv(Socket, PacketSize, 1000),
    io:format("~p got data:~p~n", [self(), Data]),
    <<1, 0, 32, _Token:32/binary,
      2, Size:16/integer, Payload:(Size)/binary,
      3, 4, MessageId:32/integer,
      _Rest/binary>> = Data,
    io:format("~p MessageId:~p Payload:~p~n", [self(), MessageId, Payload]),
    case MessageId of
        20 -> ok;
        ErrorCode -> ssl:send(Socket, <<8, ErrorCode:8/integer, MessageId:32/integer>>)
    end,
    read_data(Socket).


start_feedback() ->
    ssl:start(),
    Port = 2196,
    SSL_Options = [{certfile, "server.crt"},
                   {keyfile, "server.key"},
                   {reuseaddr, true}],
    io:format("start feedback ~p at port:~p~n", [self(), Port]),
    {ok, ListenSocket} = ssl:listen(Port, SSL_Options),
    feedback_wait_for_client(ListenSocket).


feedback_wait_for_client(ListenSocket) ->
    io:format("~p feedback wait for client~n", [self()]),
    {ok, Socket} = ssl:transport_accept(ListenSocket),
    ok = ssl:ssl_accept(Socket),
    ssl:send(Socket, data()),
    ssl:close(Socket),
    feedback_wait_for_client(ListenSocket).


data() ->
    Timestamp1 = <<84,200,111,111>>,
    Token1 = <<111, 111, 111, 111, 111, 111, 111, 111,
               111, 111, 111, 111, 111, 111, 111, 111,
               111, 111, 111, 111, 111, 111, 111, 111,
               111, 111, 111, 111, 111, 111, 111, 111>>,
    Timestamp2 = <<84,200,222,222>>,
    Token2 = <<222, 222, 222, 222, 222, 222, 222, 222,
               222, 222, 222, 222, 222, 222, 222, 222,
               222, 222, 222, 222, 222, 222, 222, 222,
               222, 222, 222, 222, 222, 222, 222, 222>>,
    <<Timestamp1/binary, 0, 32, Token1/binary,
      Timestamp2/binary, 0, 32, Token2/binary>>.
