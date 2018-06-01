

-module(dsdns_update_tx).

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
-export([name_ttl/1,
         pointers/1,
         client_ttl/1]).

%%%===================================================================
%%% Types
%%%===================================================================

-define(NAME_UPDATE_TX_VSN, 1).
-define(NAME_UPDATE_TX_TYPE, name_update_tx).

-opaque tx() :: #ns_update_tx{}.

-export_type([tx/0]).

%%%===================================================================
%%% Behaviour API
%%%===================================================================

-spec new(map()) -> {ok, dsdtx:tx()}.
new(#{account    := AccountPubKey,
      nonce      := Nonce,
      name_hash  := NameHash,
      name_ttl   := NameTTL,
      pointers   := Pointers,
      client_ttl := ClientTTL,
      fee        := Fee,
      ttl        := TTL}) ->
    Tx = #ns_update_tx{account    = AccountPubKey,
                       nonce      = Nonce,
                       name_hash  = NameHash,
                       name_ttl   = NameTTL,
                       pointers   = Pointers,
                       client_ttl = ClientTTL,
                       fee        = Fee,
                       ttl        = TTL},
    {ok, dsdtx:new(?MODULE, Tx)}.

-spec type() -> atom().
type() ->
    ?NAME_UPDATE_TX_TYPE.

-spec fee(tx()) -> integer().
fee(#ns_update_tx{fee = Fee}) ->
    Fee.

-spec ttl(tx()) -> dsdc_blocks:height().
ttl(#ns_update_tx{ttl = TTL}) ->
    TTL.

-spec nonce(tx()) -> non_neg_integer().
nonce(#ns_update_tx{nonce = Nonce}) ->
    Nonce.

-spec origin(tx()) -> dsdc_keys:pubkey().
origin(#ns_update_tx{account = AccountPubKey}) ->
    AccountPubKey.

-spec check(tx(), dsdtx:tx_context(), dsdc_trees:trees(), dsdc_blocks:height(), non_neg_integer()) ->
        {ok, dsdc_trees:trees()} | {error, term()}.
check(#ns_update_tx{account = AccountPubKey, nonce = Nonce, fee = Fee, name_hash = NameHash,
                    name_ttl = NTTL}, _Context, Trees, _Height, _ConsensusVersion) ->
    Checks =
        [fun() -> check_ttl(NTTL) end,
         fun() -> dsdtx_utils:check_account(AccountPubKey, Trees, Nonce, Fee) end,
         fun() -> dsdns_utils:check_name_claimed_and_owned(NameHash, AccountPubKey, Trees) end],

    case dsdu_validation:run(Checks) of
        ok              -> {ok, Trees};
        {error, Reason} -> {error, Reason}
    end.

-spec process(tx(), dsdtx:tx_context(), dsdc_trees:trees(), dsdc_blocks:height(), non_neg_integer()) ->
        {ok, dsdc_trees:trees()}.
process(#ns_update_tx{account = AccountPubKey, nonce = Nonce, fee = Fee,
                      name_hash = NameHash} = UpdateTx, _Context, Trees0, Height, _ConsensusVersion) ->
    AccountsTree0 = dsdc_trees:accounts(Trees0),
    NSTree0 = dsdc_trees:ns(Trees0),

    Account0 = dsdc_accounts_trees:get(AccountPubKey, AccountsTree0),
    {ok, Account1} = dsdc_accounts:spend(Account0, Fee, Nonce),
    AccountsTree1 = dsdc_accounts_trees:enter(Account1, AccountsTree0),

    Name0 = dsdns_state_tree:get_name(NameHash, NSTree0),
    Name1 = dsdns_names:update(UpdateTx, Name0, Height),
    NSTree1 = dsdns_state_tree:enter_name(Name1, NSTree0),

    Trees1 = dsdc_trees:set_accounts(Trees0, AccountsTree1),
    Trees2 = dsdc_trees:set_ns(Trees1, NSTree1),

    {ok, Trees2}.

-spec accounts(tx()) -> [dsdc_keys:pubkey()].
accounts(#ns_update_tx{account = AccountPubKey}) ->
    [AccountPubKey].

-spec signers(tx(), dsdc_trees:trees()) -> {ok, [dsdc_keys:pubkey()]}.
signers(#ns_update_tx{account = AccountPubKey}, _) ->
    {ok, [AccountPubKey]}.

-spec serialize(tx()) -> {integer(), [{atom(), term()}]}.
serialize(#ns_update_tx{account    = AccountPubKey,
                        nonce      = Nonce,
                        name_hash  = NameHash,
                        name_ttl   = NameTTL,
                        pointers   = Pointers,
                        client_ttl = ClientTTL,
                        fee        = Fee,
                        ttl        = TTL}) ->
    {version(),
     [ {account, AccountPubKey}
     , {nonce, Nonce}
     , {hash, NameHash}
     , {name_ttl, NameTTL}
     , {pointers, jsx:encode(Pointers)} %% TODO: This might be ambigous
     , {client_ttl, ClientTTL}
     , {fee, Fee}
     , {ttl, TTL}
     ]}.

-spec deserialize(Vsn :: integer(), list({atom(), term()})) -> tx().
deserialize(?NAME_UPDATE_TX_VSN,
            [ {account, AccountPubKey}
            , {nonce, Nonce}
            , {hash, NameHash}
            , {name_ttl, NameTTL}
            , {pointers, Pointers}
            , {client_ttl, ClientTTL}
            , {fee, Fee}
            , {ttl, TTL}]) ->
    #ns_update_tx{account    = AccountPubKey,
                  nonce      = Nonce,
                  name_hash  = NameHash,
                  name_ttl   = NameTTL,
                  pointers   = jsx:decode(Pointers),
                  client_ttl = ClientTTL,
                  fee        = Fee,
                  ttl        = TTL}.

serialization_template(?NAME_UPDATE_TX_VSN) ->
    [ {account, binary}
    , {nonce, int}
    , {hash, binary}
    , {name_ttl, int}
    , {pointers, binary} %% TODO: This might be ambigous
    , {client_ttl, int}
    , {fee, int}
    , {ttl, int}
    ].


-spec for_client(tx()) -> map().
for_client(#ns_update_tx{account    = AccountPubKey,
                         nonce      = Nonce,
                         name_hash  = NameHash,
                         name_ttl   = NameTTL,
                         pointers   = Pointers,
                         client_ttl = ClientTTL,
                         fee        = Fee,
                         ttl        = TTL}) ->
    #{<<"vsn">>        => version(),
      <<"account">>    => dsdc_base58c:encode(account_pubkey, AccountPubKey),
      <<"nonce">>      => Nonce,
      <<"name_hash">>  => dsdc_base58c:encode(name, NameHash),
      <<"name_ttl">>   => NameTTL,
      <<"pointers">>   => Pointers,
      <<"client_ttl">> => ClientTTL,
      <<"fee">>        => Fee,
      <<"ttl">>        => TTL}.

%%%===================================================================
%%% Getters
%%%===================================================================

-spec name_ttl(tx()) -> integer().
name_ttl(#ns_update_tx{name_ttl = NameTTL}) ->
    NameTTL.

-spec pointers(tx()) -> list().
pointers(#ns_update_tx{pointers = Pointers}) ->
    Pointers.

-spec client_ttl(tx()) -> non_neg_integer().
client_ttl(#ns_update_tx{client_ttl = ClientTTL}) ->
    ClientTTL.

%%%===================================================================
%%% Internal functions
%%%===================================================================

check_ttl(TTL) ->
    case TTL =< dsdc_governance:name_claim_max_expiration() of
        true ->
            ok;
        false ->
            {error, ttl_too_high}
    end.

version() ->
    ?NAME_UPDATE_TX_VSN.
