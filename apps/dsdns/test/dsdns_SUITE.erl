
-module(dsdns_SUITE).

%% common_test exports
-export([all/0,
         groups/0
        ]).

%% test case exports
-export([preclaim/1,
         prune_claim/1,
         preclaim_negative/1,
         claim/1,
         claim_negative/1,
         claim_race_negative/1,
         update/1,
         update_negative/1,
         transfer/1,
         transfer_negative/1,
         revoke/1,
         revoke_negative/1,
         prune_preclaim/1]).

-include_lib("common_test/include/ct.hrl").

-include_lib("apps/dsdcore/include/blocks.hrl").

-include_lib("apps/dsdns/include/ns_txs.hrl").
-include_lib("apps/dsdns/include/dsdns.hrl").

%% Distinct miner keys for working around account height check.
-define(MINER_PUBKEY_PRECLAIM, <<12301:?MINER_PUB_BYTES/unit:8>>).
-define(MINER_PUBKEY_CLAIM,    <<12302:?MINER_PUB_BYTES/unit:8>>).
-define(MINER_PUBKEY_UPDATE,   <<12303:?MINER_PUB_BYTES/unit:8>>).
-define(MINER_PUBKEY_TRANSFER, <<12304:?MINER_PUB_BYTES/unit:8>>).
-define(MINER_PUBKEY_REVOKE,   <<12305:?MINER_PUB_BYTES/unit:8>>).

%%%===================================================================
%%% Common test framework
%%%===================================================================

all() ->
    [{group, all_tests}].

groups() ->
    [
     {all_tests, [sequence], [{group, transactions}]},
     {transactions, [sequence],
      [prune_preclaim,
       prune_claim,
       preclaim,
       preclaim_negative,
       claim,
       claim_negative,
       claim_race_negative,
       update,
       update_negative,
       transfer,
       transfer_negative,
       revoke,
       revoke_negative]}
    ].

-define(NAME, <<"詹姆斯詹姆斯.test"/utf8>>).
-define(PRE_CLAIM_HEIGHT, 1).

%%%===================================================================
%%% Preclaim
%%%===================================================================

preclaim(Cfg) ->
    State = case proplists:get_value(state, Cfg) of
                undefined -> dsdns_test_utils:new_state();
                State0 -> State0
            end,
    {PubKey, S1} = dsdns_test_utils:setup_new_account(State),
    PrivKey = dsdns_test_utils:priv_key(PubKey, S1),
    Trees = dsdns_test_utils:trees(S1),
    Height = ?PRE_CLAIM_HEIGHT,
    Name = ?NAME,
    NameSalt = rand:uniform(10000),
    {ok, NameAscii} = dsdns_utils:to_ascii(Name),
    CHash = dsdns_hash:commitment_hash(NameAscii, NameSalt),

    %% Create Preclaim tx and apply it on trees
    TxSpec = dsdns_test_utils:preclaim_tx_spec(PubKey, CHash, S1),
    {ok, Tx} = dsdns_preclaim_tx:new(TxSpec),
    SignedTx = dsdtx_sign:sign(Tx, PrivKey),
    {ok, [SignedTx], Trees1} =
        dsdc_block_candidate:apply_block_txs([SignedTx], ?MINER_PUBKEY_PRECLAIM, Trees, Height, ?PROTOCOL_VERSION),
    S2 = dsdns_test_utils:set_trees(Trees1, S1),

    %% Check commitment created
    Trees2 = dsdns_test_utils:trees(S2),
    {value, C} = dsdns_state_tree:lookup_commitment(CHash, dsdc_trees:ns(Trees2)),
    CHash      = dsdns_commitments:id(C),
    PubKey     = dsdns_commitments:owner(C),

    {PubKey, Name, NameSalt, S2}.

preclaim_negative(Cfg) ->
    {PubKey, S1} = dsdns_test_utils:setup_new_account(dsdns_test_utils:new_state()),
    Trees = dsdns_test_utils:trees(S1),
    Height = 1,
    {ok, NameAscii} = dsdns_utils:to_ascii(<<"詹姆斯詹姆斯.test"/utf8>>),
    CHash = dsdns_hash:commitment_hash(NameAscii, 123),

    %% Test bad account key
    BadPubKey = <<42:32/unit:8>>,
    TxSpec1 = dsdns_test_utils:preclaim_tx_spec(BadPubKey, CHash, S1),
    {ok, Tx1} = dsdns_preclaim_tx:new(TxSpec1),
    {error, account_not_found} =
        dsdtx:check(Tx1, Trees, Height, ?PROTOCOL_VERSION),

    %% Insufficient funds
    S2 = dsdns_test_utils:set_account_balance(PubKey, 0, S1),
    Trees2 = dsdns_test_utils:trees(S2),
    TxSpec2 = dsdns_test_utils:preclaim_tx_spec(PubKey, CHash, S1),
    {ok, Tx2} = dsdns_preclaim_tx:new(TxSpec2),
    {error, insufficient_funds} =
        dsdtx:check(Tx2, Trees2, Height, ?PROTOCOL_VERSION),

    %% Test too high account nonce
    TxSpec3 = dsdns_test_utils:preclaim_tx_spec(PubKey, CHash, #{nonce => 0}, S1),
    {ok, Tx3} = dsdns_preclaim_tx:new(TxSpec3),
    {error, account_nonce_too_high} =
        dsdtx:check(Tx3, Trees, Height, ?PROTOCOL_VERSION),

    %% Test commitment already present
    {PubKey2, Name, NameSalt, S3} = preclaim(Cfg),
    {ok, NameAscii} = dsdns_utils:to_ascii(Name),
    CHash2 = dsdns_hash:commitment_hash(NameAscii, NameSalt),
    Trees3 = dsdns_test_utils:trees(S3),
    TxSpec4 = dsdns_test_utils:preclaim_tx_spec(PubKey2, CHash2, S3),
    {ok, Tx4} = dsdns_preclaim_tx:new(TxSpec4),
    {error, commitment_already_present}
        = dsdtx:check(Tx4, Trees3, Height, ?PROTOCOL_VERSION),
    ok.

%%%===================================================================
%%% Claim
%%%===================================================================

claim(Cfg) ->
    {PubKey, Name, NameSalt, S1} = preclaim(Cfg),
    Trees = dsdns_test_utils:trees(S1),
    Height = ?PRE_CLAIM_HEIGHT + 1,
    PrivKey = dsdns_test_utils:priv_key(PubKey, S1),
    {ok, NameAscii} = dsdns_utils:to_ascii(Name),
    CHash = dsdns_hash:commitment_hash(NameAscii, NameSalt),
    NHash = dsdns_hash:name_hash(NameAscii),

    %% Check commitment present
    {value, C} = dsdns_state_tree:lookup_commitment(CHash, dsdc_trees:ns(Trees)),
    CHash      = dsdns_commitments:id(C),

    %% Create Claim tx and apply it on trees
    TxSpec = dsdns_test_utils:claim_tx_spec(PubKey, Name, NameSalt, S1),
    {ok, Tx} = dsdns_claim_tx:new(TxSpec),
    SignedTx = dsdtx_sign:sign(Tx, PrivKey),

    {ok, [SignedTx], Trees1} =
        dsdc_block_candidate:apply_block_txs([SignedTx], ?MINER_PUBKEY_CLAIM, Trees, Height, ?PROTOCOL_VERSION),
    S2 = dsdns_test_utils:set_trees(Trees1, S1),

    %% Check commitment removed and name entry added
    Trees2 = dsdns_test_utils:trees(S2),
    NTrees = dsdc_trees:ns(Trees2),
    none       = dsdns_state_tree:lookup_commitment(CHash, NTrees),
    {value, N} = dsdns_state_tree:lookup_name(NHash, NTrees),
    NHash   = dsdns_names:id(N),
    PubKey  = dsdns_names:owner(N),
    claimed = dsdns_names:status(N),
    {PubKey, NHash, S2}.

claim_negative(Cfg) ->
    {PubKey, Name, NameSalt, S1} = preclaim(Cfg),
    Trees = dsdns_test_utils:trees(S1),
    Height = ?PRE_CLAIM_HEIGHT,

    %% Test commitment delta too small
    TxSpec = dsdns_test_utils:claim_tx_spec(PubKey, Name, NameSalt, S1),
    {ok, Tx0} = dsdns_claim_tx:new(TxSpec),
    {error, commitment_delta_too_small} =
        dsdtx:check(Tx0, Trees, Height, ?PROTOCOL_VERSION),

    %% Test bad account key
    BadPubKey = <<42:32/unit:8>>,
    TxSpec1 = dsdns_test_utils:claim_tx_spec(BadPubKey, Name, NameSalt, S1),
    {ok, Tx1} = dsdns_claim_tx:new(TxSpec1),
    {error, account_not_found} =
        dsdtx:check(Tx1, Trees, Height, ?PROTOCOL_VERSION),

    %% Insufficient funds
    S2 = dsdns_test_utils:set_account_balance(PubKey, 0, S1),
    Trees2 = dsdns_test_utils:trees(S2),
    TxSpec2 = dsdns_test_utils:claim_tx_spec(PubKey, Name, NameSalt, S1),
    {ok, Tx2} = dsdns_claim_tx:new(TxSpec2),
    {error, insufficient_funds} =
        dsdtx:check(Tx2, Trees2, Height, ?PROTOCOL_VERSION),

    %% Test too high account nonce
    TxSpec3 = dsdns_test_utils:claim_tx_spec(PubKey, Name, NameSalt, #{nonce => 0}, S1),
    {ok, Tx3} = dsdns_claim_tx:new(TxSpec3),
    {error, account_nonce_too_high} =
        dsdtx:check(Tx3, Trees, Height, ?PROTOCOL_VERSION),

    %% Test commitment not found
    TxSpec4 = dsdns_test_utils:claim_tx_spec(PubKey, Name, NameSalt + 1, S1),
    {ok, Tx4} = dsdns_claim_tx:new(TxSpec4),
    {error, name_not_preclaimed} =
        dsdtx:check(Tx4, Trees, Height, ?PROTOCOL_VERSION),

    %% Test commitment not owned
    {PubKey2, S3} = dsdns_test_utils:setup_new_account(S1),
    Trees3 = dsdns_test_utils:trees(S3),
    TxSpec5 = dsdns_test_utils:claim_tx_spec(PubKey2, Name, NameSalt, S3),
    {ok, Tx5} = dsdns_claim_tx:new(TxSpec5),
    {error, commitment_not_owned} =
        dsdtx:check(Tx5, Trees3, Height, ?PROTOCOL_VERSION),

    %% Test bad name
    TxSpec6 = dsdns_test_utils:claim_tx_spec(PubKey, <<"abcdefghi">>, NameSalt, S1),
    {ok, Tx6} = dsdns_claim_tx:new(TxSpec6),
    {error, no_registrar} =
        dsdtx:check(Tx6, Trees, Height, ?PROTOCOL_VERSION),
    ok.

claim_race_negative(_Cfg) ->
    %% The first claim
    {_PubKey, _NHash, S1} = claim([]),

    %% The second claim of the same name (hardcoded in preclaim) decomposed
    {PubKey2, Name2, NameSalt2, S2} = preclaim([{state, S1}]),
    Trees = dsdns_test_utils:trees(S2),
    Height = ?PRE_CLAIM_HEIGHT + 1,

    %% Test bad account key
    TxSpec1 = dsdns_test_utils:claim_tx_spec(PubKey2, Name2, NameSalt2, S2),
    {ok, Tx1} = dsdns_claim_tx:new(TxSpec1),
    {error, name_already_taken} = dsdtx:check(Tx1, Trees, Height, ?PROTOCOL_VERSION).

%%%===================================================================
%%% Update
%%%===================================================================

update(Cfg) ->
    {PubKey, NHash, S1} = claim(Cfg),
    Trees = dsdns_test_utils:trees(S1),
    Height = ?PRE_CLAIM_HEIGHT+1,
    PrivKey = dsdns_test_utils:priv_key(PubKey, S1),

    %% Check name present, but neither pointers nor name TTL set
    {value, N} = dsdns_state_tree:lookup_name(NHash, dsdc_trees:ns(Trees)),
    [] = dsdns_names:pointers(N),
    0  = dsdns_names:client_ttl(N),

    %% Create Update tx and apply it on trees
    Pointers = [{<<"account_pubkey">>, <<"dsdcore_suite_utils">>}],
    NameTTL  = 40000,
    TxSpec = dsdns_test_utils:update_tx_spec(
               PubKey, NHash, #{pointers => Pointers, name_ttl => NameTTL}, S1),
    {ok, Tx} = dsdns_update_tx:new(TxSpec),
    SignedTx = dsdtx_sign:sign(Tx, PrivKey),

    {ok, [SignedTx], Trees1} =
        dsdc_block_candidate:apply_block_txs([SignedTx], ?MINER_PUBKEY_UPDATE, Trees, Height, ?PROTOCOL_VERSION),

    %% Check name present, with both pointers and TTL set
    {value, N1} = dsdns_state_tree:lookup_name(NHash, dsdc_trees:ns(Trees1)),
    Pointers = dsdns_names:pointers(N1),
    NameTTL  = dsdns_names:expires(N1) - Height,
    ok.

update_negative(Cfg) ->
    {PubKey, NHash, S1} = claim(Cfg),
    Trees = dsdns_test_utils:trees(S1),
    Height = ?PRE_CLAIM_HEIGHT + 1,

    %% Test TX TTL too low
    MaxTTL = dsdc_governance:name_claim_max_expiration(),
    TxSpec0 = dsdns_test_utils:update_tx_spec(PubKey, NHash, #{ttl => Height - 1}, S1),
    {ok, Tx0} = dsdns_update_tx:new(TxSpec0),
    {error, ttl_expired} =
        dsdtx:check(Tx0, Trees, Height, ?PROTOCOL_VERSION),

    %% Test name TTL too high
    MaxTTL = dsdc_governance:name_claim_max_expiration(),
    TxSpec1 = dsdns_test_utils:update_tx_spec(PubKey, NHash, #{name_ttl => MaxTTL + 1}, S1),
    {ok, Tx1} = dsdns_update_tx:new(TxSpec1),
    {error, ttl_too_high} =
        dsdtx:check(Tx1, Trees, Height, ?PROTOCOL_VERSION),

    %% Test bad account key
    BadPubKey = <<42:32/unit:8>>,
    TxSpec2 = dsdns_test_utils:update_tx_spec(BadPubKey, NHash, S1),
    {ok, Tx2} = dsdns_update_tx:new(TxSpec2),
    {error, account_not_found} =
        dsdtx:check(Tx2, Trees, Height, ?PROTOCOL_VERSION),

    %% Insufficient funds
    S2 = dsdns_test_utils:set_account_balance(PubKey, 0, S1),
    Trees2 = dsdns_test_utils:trees(S2),
    TxSpec3 = dsdns_test_utils:update_tx_spec(PubKey, NHash, S1),
    {ok, Tx3} = dsdns_update_tx:new(TxSpec3),
    {error, insufficient_funds} =
        dsdtx:check(Tx3, Trees2, Height, ?PROTOCOL_VERSION),

    %% Test too high account nonce
    TxSpec4 = dsdns_test_utils:update_tx_spec(PubKey, NHash, #{nonce => 0}, S1),
    {ok, Tx4} = dsdns_update_tx:new(TxSpec4),
    {error, account_nonce_too_high} =
        dsdtx:check(Tx4, Trees, Height, ?PROTOCOL_VERSION),

    %% Test name not present
    {ok, NHash2} = dsdns:get_name_hash(<<"othername.test">>),
    TxSpec5 = dsdns_test_utils:update_tx_spec(PubKey, NHash2, S1),
    {ok, Tx5} = dsdns_update_tx:new(TxSpec5),
    {error, name_does_not_exist} =
        dsdtx:check(Tx5, Trees, Height, ?PROTOCOL_VERSION),

    %% Test name not owned
    {PubKey2, S3} = dsdns_test_utils:setup_new_account(S1),
    Trees3 = dsdns_test_utils:trees(S3),
    TxSpec6 = dsdns_test_utils:update_tx_spec(PubKey2, NHash, S3),
    {ok, Tx6} = dsdns_update_tx:new(TxSpec6),
    {error, name_not_owned} =
        dsdtx:check(Tx6, Trees3, Height, ?PROTOCOL_VERSION),

    %% Test name revoked
    {value, N} = dsdns_state_tree:lookup_name(NHash, dsdc_trees:ns(Trees)),
    S4 = dsdns_test_utils:revoke_name(N, S1),

    TxSpec7 = dsdns_test_utils:update_tx_spec(PubKey, NHash, S4),
    {ok, Tx7} = dsdns_update_tx:new(TxSpec7),
    {error, name_revoked} =
        dsdtx:check(Tx7, dsdns_test_utils:trees(S4), Height, ?PROTOCOL_VERSION),
    ok.

%%%===================================================================
%%% Transfer
%%%===================================================================

transfer(Cfg) ->
    {PubKey, NHash, S1} = claim(Cfg),
    Trees = dsdns_test_utils:trees(S1),
    Height = ?PRE_CLAIM_HEIGHT+1,
    PrivKey = dsdns_test_utils:priv_key(PubKey, S1),

    %% Check name present
    {value, N} = dsdns_state_tree:lookup_name(NHash, dsdc_trees:ns(Trees)),
    PubKey = dsdns_names:owner(N),

    %% Create Transfer tx and apply it on trees
    {PubKey2, S2} = dsdns_test_utils:setup_new_account(S1),
    Trees1 = dsdns_test_utils:trees(S2),
    TxSpec = dsdns_test_utils:transfer_tx_spec(
               PubKey, NHash, PubKey2, S1),
    {ok, Tx} = dsdns_transfer_tx:new(TxSpec),
    SignedTx = dsdtx_sign:sign(Tx, PrivKey),

    {ok, [SignedTx], Trees2} =
        dsdc_block_candidate:apply_block_txs([SignedTx], ?MINER_PUBKEY_TRANSFER, Trees1, Height, ?PROTOCOL_VERSION),

    %% Check name new owner
    {value, N1} = dsdns_state_tree:lookup_name(NHash, dsdc_trees:ns(Trees2)),
    PubKey2 = dsdns_names:owner(N1),
    ok.

transfer_negative(Cfg) ->
    {PubKey, NHash, S1} = claim(Cfg),
    Trees = dsdns_test_utils:trees(S1),
    Height = ?PRE_CLAIM_HEIGHT+1,

    %% Test bad account key
    BadPubKey = <<42:32/unit:8>>,
    TxSpec1 = dsdns_test_utils:transfer_tx_spec(BadPubKey, NHash, PubKey, S1),
    {ok, Tx1} = dsdns_transfer_tx:new(TxSpec1),
    {error, account_not_found} =
        dsdtx:check(Tx1, Trees, Height, ?PROTOCOL_VERSION),

    %% Insufficient funds
    S2 = dsdns_test_utils:set_account_balance(PubKey, 0, S1),
    Trees2 = dsdns_test_utils:trees(S2),
    TxSpec2 = dsdns_test_utils:transfer_tx_spec(PubKey, NHash, PubKey, S1),
    {ok, Tx2} = dsdns_transfer_tx:new(TxSpec2),
    {error, insufficient_funds} =
        dsdtx:check(Tx2, Trees2, Height, ?PROTOCOL_VERSION),

    %% Test too high account nonce
    TxSpec3 = dsdns_test_utils:transfer_tx_spec(PubKey, NHash, PubKey, #{nonce => 0}, S1),
    {ok, Tx3} = dsdns_transfer_tx:new(TxSpec3),
    {error, account_nonce_too_high} =
        dsdtx:check(Tx3, Trees, Height, ?PROTOCOL_VERSION),

    %% Test name not present
    {ok, NHash2} = dsdns:get_name_hash(<<"othername.test">>),
    TxSpec4 = dsdns_test_utils:transfer_tx_spec(PubKey, NHash2, PubKey, S1),
    {ok, Tx4} = dsdns_transfer_tx:new(TxSpec4),
    {error, name_does_not_exist} =
        dsdtx:check(Tx4, Trees, Height, ?PROTOCOL_VERSION),

    %% Test name not owned
    {PubKey2, S3} = dsdns_test_utils:setup_new_account(S1),
    Trees3 = dsdns_test_utils:trees(S3),
    TxSpec5 = dsdns_test_utils:transfer_tx_spec(PubKey2, NHash, PubKey, S3),
    {ok, Tx5} = dsdns_transfer_tx:new(TxSpec5),
    {error, name_not_owned} =
        dsdtx:check(Tx5, Trees3, Height, ?PROTOCOL_VERSION),

    %% Test name revoked
    {value, N} = dsdns_state_tree:lookup_name(NHash, dsdc_trees:ns(Trees)),
    S4 = dsdns_test_utils:revoke_name(N, S1),

    TxSpec6 = dsdns_test_utils:transfer_tx_spec(PubKey, NHash, PubKey, S4),
    {ok, Tx6} = dsdns_transfer_tx:new(TxSpec6),
    {error, name_revoked} =
        dsdtx:check(Tx6, dsdns_test_utils:trees(S4), Height, ?PROTOCOL_VERSION),
    ok.

%%%===================================================================
%%% Revoke
%%%===================================================================

revoke(Cfg) ->
    {PubKey, NHash, S1} = claim(Cfg),
    Trees = dsdns_test_utils:trees(S1),
    Height = ?PRE_CLAIM_HEIGHT+1,
    PrivKey = dsdns_test_utils:priv_key(PubKey, S1),

    %% Check name present
    {value, N} = dsdns_state_tree:lookup_name(NHash, dsdc_trees:ns(Trees)),
    claimed = dsdns_names:status(N),

    %% Create Transfer tx and apply it on trees
    TxSpec = dsdns_test_utils:revoke_tx_spec(PubKey, NHash, S1),
    {ok, Tx} = dsdns_revoke_tx:new(TxSpec),
    SignedTx = dsdtx_sign:sign(Tx, PrivKey),

    {ok, [SignedTx], Trees1} =
        dsdc_block_candidate:apply_block_txs([SignedTx], ?MINER_PUBKEY_REVOKE, Trees, Height, ?PROTOCOL_VERSION),

    %% Check name revoked
    {value, N1} = dsdns_state_tree:lookup_name(NHash, dsdc_trees:ns(Trees1)),
    revoked = dsdns_names:status(N1),
    ok.

revoke_negative(Cfg) ->
    {PubKey, NHash, S1} = claim(Cfg),
    Trees = dsdns_test_utils:trees(S1),
    Height = ?PRE_CLAIM_HEIGHT+1,

    %% Test bad account key
    BadPubKey = <<42:32/unit:8>>,
    TxSpec1 = dsdns_test_utils:revoke_tx_spec(BadPubKey, NHash, S1),
    {ok, Tx1} = dsdns_revoke_tx:new(TxSpec1),
    {error, account_not_found} =
        dsdtx:check(Tx1, Trees, Height, ?PROTOCOL_VERSION),

    %% Insufficient funds
    S2 = dsdns_test_utils:set_account_balance(PubKey, 0, S1),
    Trees2 = dsdns_test_utils:trees(S2),
    TxSpec2 = dsdns_test_utils:revoke_tx_spec(PubKey, NHash, S1),
    {ok, Tx2} = dsdns_revoke_tx:new(TxSpec2),
    {error, insufficient_funds} =
        dsdtx:check(Tx2, Trees2, Height, ?PROTOCOL_VERSION),

    %% Test too high account nonce
    TxSpec3 = dsdns_test_utils:revoke_tx_spec(PubKey, NHash, #{nonce => 0}, S1),
    {ok, Tx3} = dsdns_revoke_tx:new(TxSpec3),
    {error, account_nonce_too_high} =
        dsdtx:check(Tx3, Trees, Height, ?PROTOCOL_VERSION),

    %% Test name not present
    {ok, NHash2} = dsdns:get_name_hash(<<"othername.test">>),
    TxSpec4 = dsdns_test_utils:revoke_tx_spec(PubKey, NHash2, S1),
    {ok, Tx4} = dsdns_revoke_tx:new(TxSpec4),
    {error, name_does_not_exist} =
        dsdtx:check(Tx4, Trees, Height, ?PROTOCOL_VERSION),

    %% Test name not owned
    {PubKey2, S3} = dsdns_test_utils:setup_new_account(S1),
    Trees3 = dsdns_test_utils:trees(S3),
    TxSpec5 = dsdns_test_utils:revoke_tx_spec(PubKey2, NHash, S3),
    {ok, Tx5} = dsdns_revoke_tx:new(TxSpec5),
    {error, name_not_owned} =
        dsdtx:check(Tx5, Trees3, Height, ?PROTOCOL_VERSION),

    %% Test name already revoked
    {value, N} = dsdns_state_tree:lookup_name(NHash, dsdc_trees:ns(Trees)),
    S4 = dsdns_test_utils:revoke_name(N, S1),

    TxSpec6 = dsdns_test_utils:revoke_tx_spec(PubKey, NHash, S4),
    {ok, Tx6} = dsdns_revoke_tx:new(TxSpec6),
    {error, name_revoked} =
        dsdtx:check(Tx6, dsdns_test_utils:trees(S4), Height, ?PROTOCOL_VERSION),
    ok.

%%%===================================================================
%%% Prune names and commitments
%%%===================================================================

prune_preclaim(Cfg) ->
    {PubKey, Name, NameSalt, S1} = preclaim(Cfg),
    {ok, NameAscii} = dsdns_utils:to_ascii(Name),
    CHash = dsdns_hash:commitment_hash(NameAscii, NameSalt),
    Trees2 = dsdns_test_utils:trees(S1),
    {value, C} = dsdns_state_tree:lookup_commitment(CHash, dsdc_trees:ns(Trees2)),
    CHash      = dsdns_commitments:id(C),
    PubKey     = dsdns_commitments:owner(C),

    Expires = dsdns_commitments:expires(C),
    NSTree = do_prune_until(?GENESIS_HEIGHT, Expires + 1, dsdc_trees:ns(Trees2)),
    none = dsdns_state_tree:lookup_commitment(CHash, NSTree),
    ok.

prune_claim(Cfg) ->
    {PubKey, NHash, S2} = claim(Cfg),

    %% Re-pull values for this test
    Trees2 = dsdns_test_utils:trees(S2),
    NTrees = dsdc_trees:ns(Trees2),
    {value, N} = dsdns_state_tree:lookup_name(NHash, NTrees),

    NHash    = dsdns_names:id(N),
    PubKey   = dsdns_names:owner(N),
    claimed  = dsdns_names:status(N),
    Expires1 = dsdns_names:expires(N),


    NTree2 = dsdns_state_tree:prune(Expires1+1, NTrees),
    {value, N2} = dsdns_state_tree:lookup_name(NHash, NTree2),
    NHash    = dsdns_names:id(N2),
    PubKey   = dsdns_names:owner(N2),
    revoked  = dsdns_names:status(N2),
    Expires2 = dsdns_names:expires(N2),

    NTree3 = dsdns_state_tree:prune(Expires2+1, NTree2),
    none = dsdns_state_tree:lookup_name(NHash, NTree3),

    {PubKey, NHash, S2}.

do_prune_until(N1, N1, OTree) ->
    dsdns_state_tree:prune(N1, OTree);
do_prune_until(N1, N2, OTree) ->
    do_prune_until(N1 + 1, N2, dsdns_state_tree:prune(N1, OTree)).
