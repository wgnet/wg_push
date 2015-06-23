-module(wg_push_feedback).

-include("wg_push.hrl").

-export([get_feedback/2]).


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
