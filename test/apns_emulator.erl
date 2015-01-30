-module(apns_emulator).

-export([start/0]).


start() ->
    spawn(fun start_server/0),
    spawn(fun start_feedback/0),
    ok.


start_server() ->
    ssl:start(),
    Port = 2195,
    SSL_Options = [{certfile, "server.crt"},
                   {keyfile, "server.key"},
                   {reuseaddr, true}],
    io:format("start apns_emulator ~p at port:~p~n", [self(), Port]),
    {ok, ListenSocket} = ssl:listen(Port, SSL_Options),
    wait_for_client(ListenSocket).


wait_for_client(ListenSocket) ->
    io:format("~p emulator wait for client~n", [self()]),
    {ok, Socket} = ssl:transport_accept(ListenSocket),
    io:format("~p handshake~n", [self()]),
    ok = ssl:ssl_accept(Socket),
    io:format("~p client accepted~n", [self()]),
    receive_data(),
    ssl:close(Socket),
    wait_for_client(ListenSocket).


receive_data() ->
    io:format("~p wait for data~n", [self()]),
    receive
        {ssl, _, Data} ->
            io:format("~p server got data:~p~n", [self(), Data]),
            receive_data();
        {ssl_closed, _} ->
            io:format("~p client close connection ~n", [self()]),
            ok
    end.


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
