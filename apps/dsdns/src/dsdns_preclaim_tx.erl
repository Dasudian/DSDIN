

-module(dsdns_preclaim_tx).

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
-export([account/1,
         commitment/1]).

%%%===================================================================
%%% Types
%%%===================================================================

-define(NAME_PRECLAIM_TX_VSN, 1).
-define(NAME_PRECLAIM_TX_TYPE, name_preclaim_tx).

-opaque tx() :: #ns_preclaim_tx{}.

-export_type([tx/0]).

%%%===================================================================
%%% Behaviour API
%%%===================================================================

-spec new(map()) -> {ok, dsdtx:tx()}.
new(#{account    := AccountPubKey,
      nonce      := Nonce,
      commitment := Commitment,
      fee        := Fee,
      ttl        := TTL}) ->
    Tx = #ns_preclaim_tx{account    = AccountPubKey,
                         nonce      = Nonce,
                         commitment = Commitment,
                         fee        = Fee,
                         ttl        = TTL},
    {ok, dsdtx:new(?MODULE, Tx)}.

-spec type() -> atom().
type() ->
    ?NAME_PRECLAIM_TX_TYPE.

-spec fee(tx()) -> integer().
fee(#ns_preclaim_tx{fee = Fee}) ->
    Fee.

-spec ttl(tx()) -> dsdc_blocks:height().
ttl(#ns_preclaim_tx{ttl = TTL}) ->
    TTL.

-spec nonce(tx()) -> non_neg_integer().
nonce(#ns_preclaim_tx{nonce = Nonce}) ->
    Nonce.

-spec origin(tx()) -> dsdc_keys:pubkey().
origin(#ns_preclaim_tx{account = AccountPubKey}) ->
    AccountPubKey.

-spec check(tx(), dsdtx:tx_context(), dsdc_trees:trees(), dsdc_blocks:height(), non_neg_integer()) -> {ok, dsdc_trees:trees()} | {error, term()}.
check(#ns_preclaim_tx{account = AccountPubKey, nonce = Nonce,
                      fee = Fee, commitment = Commitment}, _Context, Trees, _Height, _ConsensusVersion) ->
    Checks =
        [fun() -> dsdtx_utils:check_account(AccountPubKey, Trees, Nonce, Fee) end,
         fun() -> check_not_commitment(Commitment, Trees) end],

    case dsdu_validation:run(Checks) of
        ok              -> {ok, Trees};
        {error, Reason} -> {error, Reason}
    end.

-spec process(tx(), dsdtx:tx_context(), dsdc_trees:trees(), dsdc_blocks:height(), non_neg_integer()) -> {ok, dsdc_trees:trees()}.
process(#ns_preclaim_tx{account = AccountPubKey, fee = Fee,
                        nonce = Nonce} = PreclaimTx, _Context, Trees0, Height, _ConsensusVersion) ->
    AccountsTree0 = dsdc_trees:accounts(Trees0),
    NSTree0 = dsdc_trees:ns(Trees0),

    Account0 = dsdc_accounts_trees:get(AccountPubKey, AccountsTree0),
    {ok, Account1} = dsdc_accounts:spend(Account0, Fee, Nonce),
    AccountsTree1 = dsdc_accounts_trees:enter(Account1, AccountsTree0),

    TTL = dsdc_governance:name_preclaim_expiration(),
    Commitment = dsdns_commitments:new(PreclaimTx, TTL, Height),
    NSTree1 = dsdns_state_tree:enter_commitment(Commitment, NSTree0),

    Trees1 = dsdc_trees:set_accounts(Trees0, AccountsTree1),
    Trees2 = dsdc_trees:set_ns(Trees1, NSTree1),

    {ok, Trees2}.

-spec accounts(tx()) -> [dsdc_keys:pubkey()].
accounts(#ns_preclaim_tx{account = AccountPubKey}) ->
    [AccountPubKey].

-spec signers(tx(), dsdc_trees:trees()) -> {ok, [dsdc_keys:pubkey()]}.
signers(#ns_preclaim_tx{account = AccountPubKey}, _) ->
    {ok, [AccountPubKey]}.

-spec serialize(tx()) -> {integer(), [{atom(), term()}]}.
serialize(#ns_preclaim_tx{account    = AccountPubKey,
                          nonce      = Nonce,
                          commitment = Commitment,
                          fee        = Fee,
                          ttl        = TTL}) ->
    {version(),
     [ {account, AccountPubKey}
     , {nonce, Nonce}
     , {commitment, Commitment}
     , {fee, Fee}
     , {ttl, TTL}
     ]}.

-spec deserialize(Vsn :: integer(), list({atom(), term()})) -> tx().
deserialize(?NAME_PRECLAIM_TX_VSN,
            [ {account, AccountPubKey}
            , {nonce, Nonce}
            , {commitment, Commitment}
            , {fee, Fee}
            , {ttl, TTL}]) ->
    #ns_preclaim_tx{account    = AccountPubKey,
                    nonce      = Nonce,
                    commitment = Commitment,
                    fee        = Fee,
                    ttl        = TTL}.

serialization_template(?NAME_PRECLAIM_TX_VSN) ->
    [ {account, binary}
    , {nonce, int}
    , {commitment, binary}
    , {fee, int}
    , {ttl, int}
    ].

-spec for_client(tx()) -> map().
for_client(#ns_preclaim_tx{account    = AccountPubKey,
                           nonce      = Nonce,
                           commitment = Commitment,
                           fee        = Fee,
                           ttl        = TTL}) ->
    #{<<"vsn">>        => version(),
      <<"account">>    => dsdc_base58c:encode(account_pubkey, AccountPubKey),
      <<"nonce">>      => Nonce,
      <<"commitment">> => dsdc_base58c:encode(commitment, Commitment),
      <<"fee">>        => Fee,
      <<"ttl">>        => TTL}.

%%%===================================================================
%%% Getters
%%%===================================================================

-spec account(tx()) -> dsdc_keys:pubkey().
account(#ns_preclaim_tx{account = AccountPubKey}) ->
    AccountPubKey.

-spec commitment(tx()) -> binary().
commitment(#ns_preclaim_tx{commitment = Commitment}) ->
    Commitment.

%%%===================================================================
%%% Internal functions
%%%===================================================================

check_not_commitment(Commitment, Trees) ->
    NSTree = dsdc_trees:ns(Trees),
    case dsdns_state_tree:lookup_commitment(Commitment, NSTree) of
        {value, _Commitment} -> {error, commitment_already_present};
        none -> ok
    end.

version() ->
    ?NAME_PRECLAIM_TX_VSN.
