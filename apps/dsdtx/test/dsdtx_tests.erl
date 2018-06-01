
-module(dsdintx_tests).

-include_lib("eunit/include/eunit.hrl").

-include_lib("apps/dsdcore/include/blocks.hrl").

-define(TEST_MODULE, dsdintx).

-define(RECIPIENT_PUBKEY, <<"recipient_pubkey">>).

%% Probably to be moved to common tests
apply_signed_txs_test_() ->
    {setup,
     fun() ->
             ok = meck:new(dsdc_chain, [passthrough]),
             meck:expect(dsdc_chain, get_top_state, 0, {ok, dsdc_trees:new()}),
             dsdc_test_utils:dsdc_keys_setup()
     end,
     fun(TmpKeysDir) ->
             ok = dsdc_test_utils:dsdc_keys_cleanup(TmpKeysDir),
             meck:unload(dsdc_chain)
     end,
     [{"Apply txs and add total fee to miner's account",
       fun() ->
               %% Init state tree with 2 accounts
               {ok, MinerPubkey} = dsdc_keys:pubkey(),
               MinerAccount =
                    dsdc_accounts:set_nonce(dsdc_accounts:new(MinerPubkey, 100), 10),
               AnotherAccount =
                    dsdc_accounts:set_nonce(dsdc_accounts:new(?RECIPIENT_PUBKEY, 80), 12),
               StateTree0 = dsdc_test_utils:create_state_tree_with_accounts(
                              [MinerAccount, AnotherAccount]),

               BlockHeight = 30,
               %% Create 2 signed transactions (1 valid, 1 invalid)
               {ok, SpendTx} = dsdc_spend_tx:new(
                                 #{sender => MinerPubkey,
                                   recipient => ?RECIPIENT_PUBKEY,
                                   amount => 40,
                                   fee => 9,
                                   ttl => 100,
                                   nonce => 11,
                                   payload => <<"">>}),
               {ok, OverBalanceTx} = dsdc_spend_tx:new(
                                       #{sender => MinerPubkey,
                                         recipient => ?RECIPIENT_PUBKEY,
                                         amount => 30000,
                                         fee => 10,
                                         ttl => 100,
                                         nonce => 13,
                                         payload => <<"">>}),
               {ok, SignedSpendTx} = dsdc_keys:sign(SpendTx),
               {ok, SignedOverBalanceTx} = dsdc_keys:sign(OverBalanceTx),
               SignedTxs = [SignedSpendTx, SignedOverBalanceTx],

               {ok, ValidSignedTxs, StateTree} =
                  dsdc_block_candidate:apply_block_txs(SignedTxs, MinerPubkey, StateTree0, BlockHeight, ?PROTOCOL_VERSION),

               ?assertEqual([SignedSpendTx], ValidSignedTxs),

               ResultAccountsTree = dsdc_trees:accounts(StateTree),
               {value, ResultMinerAccount} = dsdc_accounts_trees:lookup(MinerPubkey, ResultAccountsTree),
               {value, ResultRecipientAccount} = dsdc_accounts_trees:lookup(?RECIPIENT_PUBKEY, ResultAccountsTree),

               %% Initial balance - spend_tx amount - spend_tx fee + spend_tx fee + block mining reward
               ?assertEqual(100 - 40 - 9 + 9 + dsdc_governance:block_mine_reward(), dsdc_accounts:balance(ResultMinerAccount)),
               ?assertEqual(80 + 40, dsdc_accounts:balance(ResultRecipientAccount))
       end
      }]}.
