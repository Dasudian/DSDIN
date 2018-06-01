

-module(dsdns_transfer_tx).

-include("ns_txs.hrl").

-behavior(dsdtx).

%% Behavior API
-export([new/1,
         type/0,
         fee/1,
         ttl/1,
         nonce/1,
         origin/1,
         check/5,
         process/5,
         accounts/1,
         signers/2,
         serialization_template/1,
         serialize/1,
         deserialize/2,
         for_client/1
        ]).

%% Getters
-export([recipient_account/1]).

%%%===================================================================
%%% Types
%%%===================================================================

-define(NAME_TRANSFER_TX_VSN, 1).
-define(NAME_TRANSFER_TX_TYPE, name_transfer_tx).

-opaque tx() :: #ns_transfer_tx{}.

-export_type([tx/0]).

%%%===================================================================
%%% Behaviour API
%%%===================================================================

-spec new(map()) -> {ok, dsdtx:tx()}.
new(#{account           := AccountPubKey,
      nonce             := Nonce,
      name_hash         := NameHash,
      recipient_account := RecipientAccountPubKey,
      fee               := Fee,
      ttl               := TTL}) ->
    Tx = #ns_transfer_tx{account           = AccountPubKey,
                         nonce             = Nonce,
                         name_hash         = NameHash,
                         recipient_account = RecipientAccountPubKey,
                         fee               = Fee,
                         ttl               = TTL},
    {ok, dsdtx:new(?MODULE, Tx)}.

-spec type() -> atom().
type() ->
    ?NAME_TRANSFER_TX_TYPE.

-spec fee(tx()) -> integer().
fee(#ns_transfer_tx{fee = Fee}) ->
    Fee.

-spec ttl(tx()) -> dsdc_blocks:height().
ttl(#ns_transfer_tx{ttl = TTL}) ->
    TTL.

-spec nonce(tx()) -> non_neg_integer().
nonce(#ns_transfer_tx{nonce = Nonce}) ->
    Nonce.

-spec origin(tx()) -> dsdc_keys:pubkey().
origin(#ns_transfer_tx{account = AccountPubKey}) ->
    AccountPubKey.

-spec check(tx(), dsdtx:tx_context(), dsdc_trees:trees(), dsdc_blocks:height(), non_neg_integer()) -> {ok, dsdc_trees:trees()} | {error, term()}.
check(#ns_transfer_tx{account = AccountPubKey, nonce = Nonce,
                      fee = Fee, name_hash = NameHash}, _Context, Trees, _Height, _ConsensusVersion) ->
    Checks =
        [fun() -> dsdtx_utils:check_account(AccountPubKey, Trees, Nonce, Fee) end,
         fun() -> dsdns_utils:check_name_claimed_and_owned(NameHash, AccountPubKey, Trees) end],

    case dsdu_validation:run(Checks) of
        ok              -> {ok, Trees};
        {error, Reason} -> {error, Reason}
    end.

-spec process(tx(), dsdtx:tx_context(), dsdc_trees:trees(), dsdc_blocks:height(), non_neg_integer()) -> {ok, dsdc_trees:trees()}.
process(#ns_transfer_tx{account = AccountPubKey, fee = Fee,
                        name_hash = NameHash, nonce = Nonce} = TransferTx, _Context, Trees0, _Height, _ConsensusVersion) ->
    AccountsTree0 = dsdc_trees:accounts(Trees0),
    NamesTree0 = dsdc_trees:ns(Trees0),

    Account0 = dsdc_accounts_trees:get(AccountPubKey, AccountsTree0),
    {ok, Account1} = dsdc_accounts:spend(Account0, Fee, Nonce),
    AccountsTree1 = dsdc_accounts_trees:enter(Account1, AccountsTree0),

    Name0 = dsdns_state_tree:get_name(NameHash, NamesTree0),
    Name1 = dsdns_names:transfer(TransferTx, Name0),
    NamesTree1 = dsdns_state_tree:enter_name(Name1, NamesTree0),

    Trees1 = dsdc_trees:set_accounts(Trees0, AccountsTree1),
    Trees2 = dsdc_trees:set_ns(Trees1, NamesTree1),

    {ok, Trees2}.

-spec accounts(tx()) -> [dsdc_keys:pubkey()].
accounts(#ns_transfer_tx{account = AccountPubKey,
                         recipient_account = RecipientPubKey}) ->
    [AccountPubKey, RecipientPubKey].

-spec signers(tx(), dsdc_trees:trees()) -> {ok, [dsdc_keys:pubkey()]}.
signers(#ns_transfer_tx{account = AccountPubKey}, _) ->
    {ok, [AccountPubKey]}.

-spec serialize(tx()) -> {integer(), [{atom(), term()}]}.
serialize(#ns_transfer_tx{account           = AccountPubKey,
                          nonce             = Nonce,
                          name_hash         = NameHash,
                          recipient_account = RecipientAccountPubKey,
                          fee               = Fee,
                          ttl               = TTL}) ->
    {version(),
     [ {account, AccountPubKey}
     , {nonce, Nonce}
     , {hash, NameHash}
     , {recipient, RecipientAccountPubKey}
     , {fee, Fee}
     , {ttl, TTL}
     ]}.

-spec deserialize(Vsn :: integer(), list({atom(), term()})) -> tx().
deserialize(?NAME_TRANSFER_TX_VSN,
            [ {account, AccountPubKey}
            , {nonce, Nonce}
            , {hash, NameHash}
            , {recipient, RecipientAccountPubKey}
            , {fee, Fee}
            , {ttl, TTL}]) ->
    #ns_transfer_tx{account           = AccountPubKey,
                    nonce             = Nonce,
                    name_hash         = NameHash,
                    recipient_account = RecipientAccountPubKey,
                    fee               = Fee,
                    ttl               = TTL}.

serialization_template(?NAME_TRANSFER_TX_VSN) ->
    [ {account, binary}
    , {nonce, int}
    , {hash, binary}
    , {recipient, binary}
    , {fee, int}
    , {ttl, int}
    ].

-spec for_client(tx()) -> map().
for_client(#ns_transfer_tx{account           = AccountPubKey,
                           nonce             = Nonce,
                           name_hash         = NameHash,
                           recipient_account = RecipientPubKey,
                           fee               = Fee,
                           ttl               = TTL}) ->
    #{<<"vsn">>              => version(),
      <<"account">>          => dsdc_base58c:encode(account_pubkey, AccountPubKey),
      <<"nonce">>            => Nonce,
      <<"name_hash">>        => dsdc_base58c:encode(name, NameHash),
      <<"recipient_pubkey">> => dsdc_base58c:encode(account_pubkey, RecipientPubKey),
      <<"fee">>              => Fee,
      <<"ttl">>              => TTL}.

%%%===================================================================
%%% Getters
%%%===================================================================

-spec recipient_account(tx()) -> dsdc_keys:pubkey().
recipient_account(#ns_transfer_tx{recipient_account = AccountPubKey}) ->
    AccountPubKey.

%%%===================================================================
%%% Internal functions
%%%===================================================================

version() ->
    ?NAME_TRANSFER_TX_VSN.
