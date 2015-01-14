-module(wg_apns_push).

-export([send/4, send/5, get_feedback/2, get_feedback/3]).

-include("wg_push.hrl").


-spec send(binary(), device_token(), ssl_service(), ssl:path(), string()) ->
             ok | {error, message_too_big} | {error, term()}.
send(Msg, DeviceToken, SSL_Service, CertFile, Password) ->
    send(Msg, DeviceToken, SSL_Service,
         [{certfile, CertFile}, {password, Password}]).


-spec send(binary(), device_token(), ssl_service(), [ssl:ssloption()]) ->
             ok | {error, message_too_big} | {error, term()}.
send(Msg, DeviceToken, {Host, Port}, SSL_Options) ->
    MSize = byte_size(Msg),
    if
        MSize > 255 -> {error, message_too_big};
        true -> case ssl:connect(Host, Port, SSL_Options) of
                    {ok, Socket} ->
                        Payload = <<0, 0, 32, DeviceToken/binary, MSize:16/integer, Msg/binary>>,
                        ok = ssl:send(Socket, Payload),
                        ssl:close(Socket),
                        ok;
                    {error, Error} -> {error, Error}
                end
    end.


-spec get_feedback(ssl_service(), ssl:path(), string()) -> {ok, [device_token()]} | {error, term()}.
get_feedback(SSL_Service, Certfile, Password) ->
    get_feedback(SSL_Service, [{certfile, Certfile}, {password, Password}]).


-spec get_feedback(ssl_service(), ssl:ssloptions()) -> {ok, [device_token()]} | {error, term()}.
get_feedback({Host, Port}, SSL_Options) ->
    case ssl:connect(Host, Port, SSL_Options) of
        {ok, Socket} -> Data = read_feedback([]),
                        ssl:close(Socket),
                        Tokens = get_tokens(Data, []),
                        {ok, Tokens};
        {error, Error} -> {error, Error}
    end.


-spec read_feedback(list()) -> binary().
read_feedback(Data) ->
    receive
        {ssl, _, Part} -> read_feedback([Part|Data]);
        {ssl_closed, _} -> list_to_binary(lists:reverse(Data))
    end.


-spec get_tokens(binary(), list()) -> [device_token()].
get_tokens(Data, Tokens) ->
    case Data of
        <<_Time:32, Size:16/integer, Token:Size/binary, Rest/binary>> ->
            get_tokens(Rest, [Token|Tokens]);
        _ -> Tokens
    end.
