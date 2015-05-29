-module(wg_apns_push).

-export([send/4, send/5, get_feedback/2]).

-include("wg_push.hrl").


-spec send(binary(), device_token(), inet_service(), ssl_options()) -> ok | {error, term()}.
send(Msg, DeviceToken, {Host, Port}, SSL_Options) ->
    send(Msg, DeviceToken, {Host, Port}, SSL_Options, 0).

-spec send(binary(), device_token(), inet_service(), ssl_options(), integer()) -> ok | {error, term()}.
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


-spec get_feedback(inet_service(), ssl_options()) -> {ok, [device_token()]} | {error, term()}.
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


-spec parse_tokens(binary(), list()) -> [device_token()].
parse_tokens(Data, Tokens) ->
    case Data of
        <<Time:32, Size:16/integer, Token:Size/binary, Rest/binary>> ->
            parse_tokens(Rest, [{Time, Token} | Tokens]);
        _ -> Tokens
    end.
