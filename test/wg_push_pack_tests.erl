-module(wg_push_pack_tests).

-include_lib("eunit/include/eunit.hrl").
-include("wg_push.hrl").


pack_item_test() ->
    Position = 9,
    DeviceToken = <<1,1,1,1,1,1,1,1,
                    2,2,2,2,2,2,2,2,
                    3,3,3,3,3,3,3,3,
                    4,4,4,4,4,4,4,4>>,
    Payload = <<"{\"alert\":\"hello\"}">>,
    Id = 777,
    EDate = 12345,
    Priority = 5,
    Item = #wg_push_item{id = Id,
                         device_token = DeviceToken,
                         payload = Payload,
                         expiration_date = EDate,
                         priority = Priority},
    Size = 32 + 17 + 4 + 4 + 1,
    Bin = <<Position:8/integer, Size:16/integer,
            DeviceToken/binary, Payload/binary, Id:32/integer, EDate:32/integer, Priority:8/integer>>,
    ?assertEqual({ok, Bin}, wg_push_pack:pack_item(Position, Item)),

    InvalidToken = <<1,2,3>>,
    Item2 = #wg_push_item{id = Id,
                          device_token = InvalidToken,
                          payload = Payload,
                          expiration_date = EDate,
                          priority = Priority},
    ?assertEqual({error, Position, invalid_device_token}, wg_push_pack:pack_item(Position, Item2)),

    InvalidPayload = lists:foldl(fun(Num, Acc) ->
                                         <<Acc/binary, Num:32/integer, DeviceToken/binary>>
                                 end,
                                 <<>>,
                                 lists:seq(1, 100)),
    Item3 = #wg_push_item{id = Id,
                          device_token = DeviceToken,
                          payload = InvalidPayload,
                          expiration_date = EDate,
                          priority = Priority},
    ?assertEqual({error, Position, payload_too_big}, wg_push_pack:pack_item(Position, Item3)),
    ok.


pack_items_test() ->
    Position = 9,
    DeviceToken = <<1,1,1,1,1,1,1,1,
                    2,2,2,2,2,2,2,2,
                    3,3,3,3,3,3,3,3,
                    4,4,4,4,4,4,4,4>>,
    Payload = <<"{\"alert\":\"hello\"}">>,
    EDate = 12345,
    Priority = 5,
    Item1 = #wg_push_item{id = 1,
                          device_token = DeviceToken,
                          payload = Payload,
                          expiration_date = EDate,
                          priority = Priority},
    {ok, Bin1} = wg_push_pack:pack_item(1, Item1),
    Item2 = #wg_push_item{id = 2,
                          device_token = DeviceToken,
                          payload = Payload,
                          expiration_date = EDate,
                          priority = Priority},
    {ok, Bin2} = wg_push_pack:pack_item(2, Item2),
    Size = byte_size(Bin1) + byte_size(Bin2),
    Res = <<2, Size:32/integer, Bin1/binary, Bin2/binary>>,
    ?assertEqual({ok, Res, []}, wg_push_pack:pack_items([Item1, Item2])),

    ?assertEqual({error, no_data}, wg_push_pack:pack_items([])),

    Item3 = #wg_push_item{id = 3,
                          device_token = <<1,2,3>>,
                          payload = Payload,
                          expiration_date = EDate,
                          priority = Priority},
    ?assertEqual({ok, Res, [{error, 3, invalid_device_token}]}, wg_push_pack:pack_items([Item1, Item2, Item3])),

    ?assertEqual({error, [{error, 1, invalid_device_token}, {error, 2, invalid_device_token}]},
                 wg_push_pack:pack_items([Item3, Item3])),


    ok.
