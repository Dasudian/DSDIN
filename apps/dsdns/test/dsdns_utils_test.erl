
-module(dsdns_utils_test).

-include_lib("eunit/include/eunit.hrl").

to_ascii_test() ->
    ?assertEqual({error, no_registrar},
                 dsdns_utils:to_ascii(<<"abcdefg">>)),
    ?assertEqual({error, multiple_namespaces},
                 dsdns_utils:to_ascii(<<"abcd.efgh.dsdt">>)),
    ?assertEqual({error, registrar_unknown},
                 dsdns_utils:to_ascii(<<"abcd.dsdttt">>)),
    LongName = base64:encode(crypto:strong_rand_bytes(1000)),
    ?assertEqual({error, name_too_long},
                 dsdns_utils:to_ascii(<<LongName/binary, ".dsdt">>)),
    ?assertEqual({error, no_label_in_registrar},
                 dsdns_utils:to_ascii(<<".dsdt">>)),
    LongLabel = base64:encode(crypto:strong_rand_bytes(80)),
    ?assertEqual({error, bad_label_length},
                 dsdns_utils:to_ascii(<<LongLabel/binary, ".dsdt">>)),
    ?assertEqual({ok, <<"xn--8wsa062gba0028bca.test">>},
                 dsdns_utils:to_ascii(<<"詹姆斯詹姆斯.test"/utf8>>)).
