%%
%% Pack APNS messages to binary
%% with APNS v2 format
%% see https://developer.apple.com/library/ios/documentation/NetworkingInternet/Conceptual/RemoteNotificationsPG/Chapters/CommunicatingWIthAPS.html
%%

-module(wg_push_pack).

-include("wg_push.hrl").

-export([pack_items/1, pack_item/1, build_ssl_options/1]).
-export([encode_aps/1, encode_alert/1]).

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
                        payload = PayloadData,
                        expiration_date = EDate,
                        priority = Priority}) ->
    Payload = encode_payload(PayloadData),
    if
        byte_size(Payload) > 2048 -> {error, Id, payload_too_big};
        byte_size(DeviceToken) /= 32 -> {error, Id, invalid_device_token};
        true ->
            Data = <<1, 0, 32, DeviceToken/binary,
					 2, (byte_size(Payload)):16/integer, Payload/binary,
					 3, 0, 4, Id:32/integer,
					 4, 0, 4, EDate:32/integer,
					 5, 0, 1, Priority:8/integer>>,
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

-spec encode_payload(#wg_push_aps{} | binary()) -> binary().
encode_payload(Payload) when is_binary(Payload) -> Payload;
encode_payload(Payload)                         -> encode_aps(Payload).

-spec encode_aps(#wg_push_aps{}) -> binary().
encode_aps(#wg_push_aps{alert = Alert, badge = Badge, sound = Sound, content_available = CA, data = Data}) ->
    Props = [
        {alert, encode_alert(Alert)},
        {badge, Badge},
        {sound, Sound},
        {'content-available', CA}
    ],
    case Data of
        undefined   ->
            jiffy:encode({[{aps, {remove_empty(Props)}}]});
        _           ->
            jiffy:encode({lists:concat([[{aps, {remove_empty(Props)}}], Data])})
    end.

-spec encode_alert(#wg_push_alert{} | binary()) -> proplists:proplist().
encode_alert(Alert) when is_binary(Alert) -> Alert;
encode_alert(#wg_push_alert{title = Title, body = Body, title_loc_key = TLK, title_loc_args = TLA,
    action_loc_key = ALK, loc_key = LK, loc_args = LA, launch_image = LI}) ->
    Props = [
        {title, Title},
        {body, Body},
        {'title-loc-key', TLK},
        {'title-loc-args', TLA},
        {'action-loc-key', ALK},
        {'loc-key', LK},
        {'loc-args', LA},
        {'launch-image', LI}
    ],
    {remove_empty(Props)}.


-spec remove_empty(proplists:proplist()) -> proplists:proplist().
remove_empty(Props) ->
    F = fun({_K, []})           -> false;
           ({_K, undefined})    -> false;
           ({_K, <<"">>})       -> false;
           (_)                  -> true
    end,
    lists:filtermap(F, Props).
