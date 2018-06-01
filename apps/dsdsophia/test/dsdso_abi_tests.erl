-module(dsdso_abi_tests).

-include_lib("eunit/include/eunit.hrl").

encode_call_with_integer_test() ->
    [64, 128, 42, 4, "main"] =
        dsdso_test_utils:dump_words(
            dsdso_abi:create_calldata("", "main", "42")).
