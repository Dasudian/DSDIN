
-module(dsdc_spend_tx_tests).

-include_lib("eunit/include/eunit.hrl").

-include("blocks.hrl").

-define(TEST_MODULE, dsdc_spend_tx).

-define(SENDER_PUBKEY, <<"sender_pubkey">>).
-define(RECIPIENT_PUBKEY, <<"recipient_pubkey">>).

check_test_() ->
    [{"Tx fee lower than minimum fee defined in governance",
      fun() ->
              {ok, SpendTx} = spend_tx(#{fee => 0, %% minimum governance fee = 1
                                         payload => <<"">>}),
              StateTree = dsdc_test_utils:create_state_tree(),
              ?assertEqual({error, too_low_fee},
                           dsdtx:check(SpendTx, StateTree, 10, ?PROTOCOL_VERSION))
      end},
     {"Sender account does not exist in state trees",
      fun() ->
              {ok, SpendTx} = spend_tx(#{fee => 10, sender => <<42>>,
                                         payload => <<"">>}),
              StateTree = dsdc_test_utils:create_state_tree(),
              ?assertEqual({error, account_not_found},
                           dsdtx:check(SpendTx, StateTree, 10, ?PROTOCOL_VERSION))
      end},
     {"Sender account has insufficient funds to cover tx fee + amount",
      fun() ->
              {ok, SpendTx} = spend_tx(#{sender => ?SENDER_PUBKEY,
                                         fee => 10,
                                         amount => 50,
                                         nonce => 12,
                                         payload => <<"">>}),

              %% Dispatcher sanity check:
              ?assertEqual(?SENDER_PUBKEY, dsdtx:origin(SpendTx)),
              ?assertEqual(12, dsdtx:nonce(SpendTx)),
              ?assertEqual(10, dsdtx:fee(SpendTx)),

              SenderAccount = new_account(#{pubkey => ?SENDER_PUBKEY, balance => 55, nonce => 5}),
              StateTree = dsdc_test_utils:create_state_tree_with_account(SenderAccount),
              ?assertEqual({error, insufficient_funds},
                           dsdtx:check(SpendTx, StateTree, 20, ?PROTOCOL_VERSION))
      end},
     {"Sender account has nonce higher than tx nonce",
      fun() ->
              {ok, SpendTx} = spend_tx(#{sender => ?SENDER_PUBKEY,
                                         fee => 10,
                                         amount => 50,
                                         nonce => 12,
                                         payload => <<"">>}),
              AccountNonce = 15,
              SenderAccount = new_account(#{pubkey => ?SENDER_PUBKEY, balance => 100, nonce => AccountNonce}),
              StateTree = dsdc_test_utils:create_state_tree_with_account(SenderAccount),
              ?assertEqual({error, account_nonce_too_high},
                           dsdtx:check(SpendTx, StateTree, 20, ?PROTOCOL_VERSION))
      end},
      {"TX TTL is too small",
      fun() ->
              {ok, SpendTx} = spend_tx(#{sender => ?SENDER_PUBKEY,
                                         fee => 10,
                                         amount => 50,
                                         ttl => 10,
                                         nonce => 11,
                                         payload => <<"">>}),
              AccountNonce = 10,
              SenderAccount = new_account(#{pubkey => ?SENDER_PUBKEY, balance => 100, nonce => AccountNonce}),
              StateTree = dsdc_test_utils:create_state_tree_with_account(SenderAccount),
              ?assertEqual({error, ttl_expired},
                           dsdtx:check(SpendTx, StateTree, 20, ?PROTOCOL_VERSION))
      end}].

process_test_() ->
    [{"Check and process valid spend tx",
      fun() ->
              SenderAccount = new_account(#{pubkey => ?SENDER_PUBKEY, balance => 100, nonce => 10}),
              RecipientAccount = new_account(#{pubkey => ?RECIPIENT_PUBKEY, balance => 80, nonce => 12}),
              StateTree0 = dsdc_test_utils:create_state_tree_with_accounts([SenderAccount, RecipientAccount]),

              {ok, SpendTx} = ?TEST_MODULE:new(#{sender => ?SENDER_PUBKEY,
                                                 recipient => ?RECIPIENT_PUBKEY,
                                                 amount => 50,
                                                 fee => 10,
                                                 ttl => 100,
                                                 nonce => 11,
                                                 payload => <<"foo">>}),
              <<"foo">> = dsdc_spend_tx:payload(dsdtx:tx(SpendTx)),
              {ok, StateTree0} = dsdtx:check(SpendTx, StateTree0, 20, ?PROTOCOL_VERSION),
              {ok, StateTree} = dsdtx:process(SpendTx, StateTree0, 20, ?PROTOCOL_VERSION),

              ResultAccountsTree = dsdc_trees:accounts(StateTree),
              {value, ResultSenderAccount} = dsdc_accounts_trees:lookup(?SENDER_PUBKEY, ResultAccountsTree),
              {value, ResultRecipientAccount} = dsdc_accounts_trees:lookup(?RECIPIENT_PUBKEY, ResultAccountsTree),

              ?assertEqual(100 - 50 - 10, dsdc_accounts:balance(ResultSenderAccount)),
              ?assertEqual(11, dsdc_accounts:nonce(ResultSenderAccount)),
              ?assertEqual(80 + 50, dsdc_accounts:balance(ResultRecipientAccount)),
              ?assertEqual(12, dsdc_accounts:nonce(ResultRecipientAccount))
      end},
      {"Check spend to oneself",
       fun() ->
              SenderAccount = new_account(#{pubkey => ?SENDER_PUBKEY, balance => 100, nonce => 10}),
              StateTree0 = dsdc_test_utils:create_state_tree_with_accounts([SenderAccount]),

              {ok, SpendTx} = ?TEST_MODULE:new(#{sender => ?SENDER_PUBKEY,
                                                 recipient => ?SENDER_PUBKEY,
                                                 amount => 50,
                                                 fee => 10,
                                                 ttl => 100,
                                                 nonce => 11,
                                                 payload => <<"foo">>}),
              {ok, StateTree0} = dsdtx:check(SpendTx, StateTree0, 20, ?PROTOCOL_VERSION),
              {ok, StateTree} = dsdtx:process(SpendTx, StateTree0, 20, ?PROTOCOL_VERSION),

              ResultAccountsTree = dsdc_trees:accounts(StateTree),
              {value, ResultAccount} = dsdc_accounts_trees:lookup(?SENDER_PUBKEY, ResultAccountsTree),

              ?assertEqual(100 - 50 - 10 + 50, dsdc_accounts:balance(ResultAccount)),
              ?assertEqual(11, dsdc_accounts:nonce(ResultAccount))
      end}].

spend_tx(Data) ->
    DefaultData = #{sender => ?SENDER_PUBKEY,
                    recipient => ?RECIPIENT_PUBKEY,
                    amount => 0,
                    fee => 0,
                    ttl => 100,
                    nonce => 0},
    dsdc_spend_tx:new(maps:merge(DefaultData, Data)).

new_account(Map) ->
    dsdc_accounts:set_nonce(
        dsdc_accounts:new(maps:get(pubkey, Map),
                         maps:get(balance, Map, 0)), maps:get(nonce, Map, 0)).
