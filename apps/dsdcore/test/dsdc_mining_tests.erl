
-module(dsdc_mining_tests).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

-include("blocks.hrl").

-define(TEST_MODULE, dsdc_mining).
-define(LOWEST_TARGET_SCI, 16#01010000).
-define(TEST_PUB, <<4,176,10,241,172,223,229,80,244,222,165,8,198,46,
                    167,128,25,34,151,180,162,192,72,103,185,62,161,12,
                    117,147,72,68,194,188,89,248,81,212,197,21,193,74,
                    115,216,210,123,239,69,164,128,164,122,116,151,23,
                    22,56,146,73,13,29,198,110,162,145>>).

mine_block_test_() ->
    {foreach,
      fun() -> setup() end,
      fun(_) -> cleanup(unused_arg) end,
      [
       {timeout, 60,
        {"Find a new block",
         fun() ->
                 TopBlock = #block{height = ?GENESIS_HEIGHT,
                                   target = ?HIGHEST_TARGET_SCI,
                                   version = ?GENESIS_VERSION},
                 % if there is a change in the structure of the block
                 % this will result in a change in the hash of the header
                 % and will invalidate the nonce value below
                 % in order to find a proper nonce for your
                 % block uncomment the line below
                 %let_it_crash = generate_valid_test_data(TopBlock, 100000000000000),
                 Nonce = 3650718748619880306,

                 {BlockCandidate, _} = dsdc_block_candidate:create_with_state(TopBlock, ?TEST_PUB,
                                                                             [], dsdc_trees:new()),
                 HeaderBin = dsdc_headers:serialize_to_binary(dsdc_blocks:to_header(BlockCandidate)),
                 Target = dsdc_blocks:target(BlockCandidate),
                 {ok, {Nonce1, Evd}} = ?TEST_MODULE:mine(HeaderBin, Target, Nonce),

                 Block = dsdc_blocks:set_pow(BlockCandidate, Nonce1, Evd),

                 ?assertEqual(1, Block#block.height),
                 ?assertEqual(0, length(Block#block.txs)),

                 ?assertEqual(ok, dsdc_headers:validate(
                                    dsdc_blocks:to_header(Block)))
         end}},
       {timeout, 60,
        {"Proof of work fails with no_solution",
         fun() ->
                 TopBlock = #block{height = ?GENESIS_HEIGHT,
                                   target = ?LOWEST_TARGET_SCI,
                                   version = ?GENESIS_VERSION},
                 meck:expect(dsdc_pow, pick_nonce, 0, 18),
                 {BlockCandidate, _} = dsdc_block_candidate:create_with_state(TopBlock, ?TEST_PUB,
                                                                             [], dsdc_trees:new()),
                 Nonce = 18,
                 HeaderBin = dsdc_headers:serialize_to_binary(dsdc_blocks:to_header(BlockCandidate)),
                 Target = dsdc_blocks:target(BlockCandidate),
                 ?assertEqual({error, no_solution},
                              ?TEST_MODULE:mine(HeaderBin, Target, Nonce))
         end}}
      ]}.


setup() ->
    ok = meck:new(dsdu_env, [passthrough]),
    dsdc_test_utils:mock_fast_and_deterministic_cuckoo_pow(),
    dsdc_test_utils:start_chain_db(),
    ok = application:ensure_started(erlexec),
    application:start(crypto),
    meck:new(dsdc_blocks, [passthrough]),
    meck:new(dsdc_headers, [passthrough]),
    meck:new(dsdtx_sign, [passthrough]),
    meck:new(dsdc_governance, [passthrough]),
    meck:new(dsdc_keys,[passthrough]),
    meck:new(dsdc_trees, [passthrough]),
    meck:new(dsdu_time, [passthrough]),
    meck:expect(dsdu_time, now_in_msecs, 0, 1519659148405),
    {ok, _} = dsdc_tx_pool:start_link(),
    Trees =
    dsdc_test_utils:create_state_tree_with_account(dsdc_accounts:new(?TEST_PUB, 0)),
    meck:expect(dsdc_trees, hash, 1, <<>>),
    meck:expect(dsdc_trees, apply_txs_on_state_trees, 4, {ok, [], Trees}),
    meck:expect(dsdc_keys, pubkey, 0, {ok, ?TEST_PUB}),
    ok.


cleanup(_) ->
    application:stop(crypto),
    meck:unload(dsdc_blocks),
    meck:unload(dsdc_headers),
    meck:unload(dsdtx_sign),
    meck:unload(dsdc_governance),
    meck:unload(dsdc_keys),
    meck:unload(dsdc_trees),
    meck:unload(dsdu_time),
    ok = dsdc_tx_pool:stop(),
    dsdc_test_utils:stop_chain_db(),
    ok = meck:unload(dsdu_env).

generate_valid_test_data(_TopBlock, Tries) when Tries < 1 ->
    could_not_find_nonce;
generate_valid_test_data(TopBlock, Tries) ->
    {ok, BlockCandidate, Nonce} = ?TEST_MODULE:create_block_candidate(TopBlock, dsdc_trees:new(), []),
    HeaderBin = dsdc_headers:serialize_to_binary(dsdc_blocks:to_header(BlockCandidate)),
    Target = dsdc_blocks:target(BlockCandidate),
    case ?TEST_MODULE:mine(HeaderBin, Target, Nonce) of
        {ok, {Nonce1, _Evd}} ->
            {ok, BlockCandidate, Nonce1};
        {error, no_solution} ->
            generate_valid_test_data(TopBlock, Tries -1)
    end.

-endif.
