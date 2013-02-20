-module(hm_uuid).
-export([uuid_random/0,uuid_utc/0]).
%% UUID tools
-spec uuid_random() -> binary().
uuid_random() ->
    Random = crypto:rand_bytes(16),
    UUID = hm_misc:to_hex(Random),
    list_to_binary(UUID).

-spec uuid_utc() -> binary().
uuid_utc() ->    
    Now = hm_date:time_micro(),
    Prefix = io_lib:format("~14.16.0b", [Now]),
    Random = crypto:rand_bytes(9),
    Hex = hm_misc:to_hex(Random),
    list_to_binary(Prefix ++ Hex).

