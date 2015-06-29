%%
%% Pack APNS messages to binary
%% with APNS v2 format
%% see https://developer.apple.com/library/ios/documentation/NetworkingInternet/Conceptual/RemoteNotificationsPG/Chapters/CommunicatingWIthAPS.html
%%

-module(wg_push_pack).

-include("wg_push.hrl").

-export([pack_items/1, pack_item/1, build_ssl_options/1]).

-type(item_error() :: {error, integer(), atom()}).

-spec pack_items([#wg_push_item{}]) -> {ok, binary()} | item_error() | {error, no_data}.
pack_items(Items) ->
    pack_items(Items, <<>>).


pack_items([], <<>>) -> {error, no_data};

pack_items([], Data) ->
    Size = byte_size(Data),
    {ok, <<2, Size:32/integer, Data/binary>>};

pack_items([Item | Rest], Data) ->
    case pack_item(Item) of
        {ok, Bin} -> pack_items(Rest, <<Data/binary, Bin/binary>>);
        ItemError -> ItemError
    end.

-spec pack_item(#wg_push_item{}) -> {ok, binary()} | item_error().
pack_item(#wg_push_item{id = Id,
                        device_token = DeviceToken,
                        payload = Payload,
                        expiration_date = EDate,
                        priority = Priority}) ->
    if
        byte_size(Payload) > 2048 -> {error, Id, payload_too_big};
        byte_size(DeviceToken) /= 32 -> {error, Id, invalid_device_token};
        true ->
            Data = <<1, 0, 32, DeviceToken/binary,
					 2, (byte_size(Payload)):16/integer, Payload/binary,
					 3, 4, Id:32/integer,
					 4, 4, EDate:32/integer,
					 5, 1, Priority:8/integer>>,
            {ok, Data}
    end.


-spec build_ssl_options(#wg_push_ssl_options{}) -> list().
build_ssl_options(#wg_push_ssl_options{certfile = CertFile, keyfile = KeyFile,
                                       cert = Cert, key = Key,
                                       password = Password, versions = Versions}) ->
    lists:filter(fun({_, undefined}) -> false;
                    (_) -> true
                 end,
                 [{certfile, CertFile},
                  {keyfile, KeyFile},
                  {cert, Cert},
                  {key, Key},
                  {password, Password},
                  {versions, Versions}]).
