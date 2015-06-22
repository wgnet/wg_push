-module(wg_apns_push).

-export([send/4, send/5, get_feedback/2]).

-include("wg_push.hrl").


send(Msg, DeviceToken, {Host, Port}, SSL_Options) ->
    send(Msg, DeviceToken, {Host, Port}, SSL_Options, 0).

send(Msg, DeviceToken, {Host, Port}, SSL_Options, ExpirationDate) ->
    MSize = byte_size(Msg),
    if
        MSize > 255 -> {error, message_too_big};
        true -> case ssl:connect(Host, Port, SSL_Options) of
                    {ok, Socket} ->
                        Protocol = 1,
                        Identifier = 1,
                        Payload = <<Protocol:8/integer,
                                    Identifier:32/integer,
                                    ExpirationDate:32/integer,
                                    0, 32, DeviceToken/binary,
                                    MSize:16/integer, Msg/binary>>,
                        ok = ssl:send(Socket, Payload),
                        ssl:close(Socket),
                        ok;
                    {error, Error} -> {error, Error}
                end
    end.


get_feedback({Host, Port}, SSL_Options) ->
    case ssl:connect(Host, Port, SSL_Options) of
        {ok, Socket} -> Data = read_reply([]),
                        ssl:close(Socket),
                        Tokens = parse_tokens(Data, []),
                        {ok, Tokens};
        {error, Error} -> {error, Error}
    end.


-spec read_reply(list()) -> binary().
read_reply(Data) ->
    receive
        {ssl, _, Part} -> read_reply([Part|Data]);
        {ssl_closed, _} -> list_to_binary(lists:reverse(Data))
    end.


parse_tokens(Data, Tokens) ->
    case Data of
        <<Time:32, Size:16/integer, Token:Size/binary, Rest/binary>> ->
            parse_tokens(Rest, [{Time, Token} | Tokens]);
        _ -> Tokens
    end.
