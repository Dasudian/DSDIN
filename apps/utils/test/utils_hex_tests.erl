%%%=============================================================================
%%% @copyright (C) 2018, Dasudian Technologies
%%% @doc
%%%   Unit tests for the utils_requests module
%%% @end
%%%=============================================================================
-module(utils_hex_tests).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

all_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     [{"Hex to binary",
       fun() ->
               ?assertEqual(<<1,2,100,255,128,12,64,123>>, utils_hex:hex_to_bin("010264ff800c407b"))
       end},
      {"Binary to hex",
       fun() ->
               ?assertEqual("010264FF800C407B", utils_hex:bin_to_hex(<<1,2,100,255,128,12,64,123>>))
        end}
     ]
    }.

setup() ->
    ok.
teardown(_) ->
    ok.
-endif.
