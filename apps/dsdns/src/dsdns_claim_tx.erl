

-module(dsdns_claim_tx).

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
         name/1]).

%%%===================================================================
%%% Types
%%%===================================================================

-define(NAME_CLAIM_TX_VSN, 1).
-define(NAME_CLAIM_TX_TYPE, name_claim_tx).

-opaque tx() :: #ns_claim_tx{}.

-export_type([tx/0]).

%%%===================================================================
%%% Behaviour API
%%%===================================================================

-spec new(map()) -> {ok, dsdtx:tx()}.
new(#{account   := AccountPubKey,
      nonce     := Nonce,
      name      := Name,
      name_salt := NameSalt,
      fee       := Fee,
      ttl       := TTL}) ->
    Tx = #ns_claim_tx{account   = AccountPubKey,
                      nonce     = Nonce,
                      name      = Name,
                      name_salt = NameSalt,
                      fee       = Fee,
                      ttl       = TTL},
    {ok, dsdtx:new(?MODULE, Tx)}.

-spec type() -> atom().
type() ->
    ?NAME_CLAIM_TX_TYPE.

-spec fee(tx()) -> integer().
fee(#ns_claim_tx{fee = Fee}) ->
    Fee.

-spec ttl(tx()) -> dsdc_blocks:height().
ttl(#ns_claim_tx{ttl = TTL}) ->
    TTL.

-spec nonce(tx()) -> non_neg_integer().
nonce(#ns_claim_tx{nonce = Nonce}) ->
    Nonce.

-spec origin(tx()) -> dsdc_keys:pubkey().
origin(#ns_claim_tx{account = AccountPubKey}) ->
    AccountPubKey.

-spec check(tx(), dsdtx:tx_context(), dsdc_trees:trees(), dsdc_blocks:height(), non_neg_integer()) -> {ok, dsdc_trees:trees()} | {error, term()}.
check(#ns_claim_tx{account = AccountPubKey, nonce = Nonce,
                   fee = Fee, name = Name, name_salt = NameSalt}, _Context, Trees, Height, _ConsensusVersion) ->
    case dsdns_utils:to_ascii(Name) of
        {ok, NameAscii} ->
            %% TODO: Maybe include burned fee in tx fee. To do so, mechanism determining
            %% which part of fee goes to miner and what gets burned is needed.
            %% One option is to change tx:fee/1 callback to tx:fee_for_miner/1.
            BurnedFee = dsdc_governance:name_claim_burned_fee(),

            Checks =
                [fun() -> dsdtx_utils:check_account(AccountPubKey, Trees, Nonce, Fee + BurnedFee) end,
                 fun() -> check_commitment(NameAscii, NameSalt, AccountPubKey, Trees, Height) end,
                 fun() -> check_name(NameAscii, Trees) end],

            case dsdu_validation:run(Checks) of
                ok              -> {ok, Trees};
                {error, Reason} -> {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

-spec process(tx(), dsdtx:tx_context(), dsdc_trees:trees(), dsdc_blocks:height(), non_neg_integer()) -> {ok, dsdc_trees:trees()}.
process(#ns_claim_tx{account = AccountPubKey, nonce = Nonce, fee = Fee,
                     name = PlainName, name_salt = NameSalt} = ClaimTx, _Context, Trees0, Height, _ConsensusVersion) ->
    AccountsTree0 = dsdc_trees:accounts(Trees0),
    NSTree0 = dsdc_trees:ns(Trees0),

    TotalFee = Fee + dsdc_governance:name_claim_burned_fee(),
    Account0 = dsdc_accounts_trees:get(AccountPubKey, AccountsTree0),
    {ok, Account1} = dsdc_accounts:spend(Account0, TotalFee, Nonce),
    AccountsTree1 = dsdc_accounts_trees:enter(Account1, AccountsTree0),

    {ok, PlainNameAscii} = dsdns_utils:to_ascii(PlainName),
    CommitmentHash = dsdns_hash:commitment_hash(PlainNameAscii, NameSalt),
    NSTree1 = dsdns_state_tree:delete_commitment(CommitmentHash, NSTree0),

    TTL = dsdc_governance:name_claim_max_expiration(),
    Name = dsdns_names:new(ClaimTx, TTL, Height),
    NSTree2 = dsdns_state_tree:enter_name(Name, NSTree1),

    Trees1 = dsdc_trees:set_accounts(Trees0, AccountsTree1),
    Trees2 = dsdc_trees:set_ns(Trees1, NSTree2),

    {ok, Trees2}.

-spec accounts(tx()) -> [dsdc_keys:pubkey()].
accounts(#ns_claim_tx{account = AccountPubKey}) ->
    [AccountPubKey].

-spec signers(tx(), dsdc_trees:trees()) -> {ok, [dsdc_keys:pubkey()]}.
signers(#ns_claim_tx{account = AccountPubKey}, _) ->
    {ok, [AccountPubKey]}.

-spec serialize(tx()) -> {integer(), [{atom(), term()}]}.
serialize(#ns_claim_tx{account   = AccountPubKey,
                       nonce     = None,
                       name      = Name,
                       name_salt = NameSalt,
                       fee       = Fee,
                       ttl       = TTL}) ->
    {version(),
     [ {account, AccountPubKey}
     , {nonce, None}
     , {name, Name}
     , {name_salt, NameSalt}
     , {fee, Fee}
     , {ttl, TTL}
     ]}.

-spec deserialize(Vsn :: integer(), list({atom(), term()})) -> tx().
deserialize(?NAME_CLAIM_TX_VSN,
            [ {account, AccountPubKey}
            , {nonce, Nonce}
            , {name, Name}
            , {name_salt, NameSalt}
            , {fee, Fee}
            , {ttl, TTL}]) ->
    #ns_claim_tx{account   = AccountPubKey,
                 nonce     = Nonce,
                 name      = Name,
                 name_salt = NameSalt,
                 fee       = Fee,
                 ttl       = TTL}.

serialization_template(?NAME_CLAIM_TX_VSN) ->
    [ {account, binary}
    , {nonce, int}
    , {name, binary}
    , {name_salt, int}
    , {fee, int}
    , {ttl, int}
    ].


-spec for_client(tx()) -> map().
for_client(#ns_claim_tx{account   = AccountPubKey,
                        nonce     = Nonce,
                        name      = Name,
                        name_salt = NameSalt,
                        fee       = Fee,
                        ttl       = TTL}) ->
    #{<<"vsn">>       => version(),
      <<"account">>   => dsdc_base58c:encode(account_pubkey, AccountPubKey),
      <<"nonce">>     => Nonce,
      <<"name">>      => Name,
      <<"name_salt">> => NameSalt,
      <<"fee">>       => Fee,
      <<"ttl">>       => TTL}.

%%%===================================================================
%%% Getters
%%%===================================================================

-spec account(tx()) -> dsdc_keys:pubkey().
account(#ns_claim_tx{account = AccountPubKey}) ->
    AccountPubKey.

-spec name(tx()) -> binary().
name(#ns_claim_tx{name = Name}) ->
    Name.

%%%===================================================================
%%% Internal functions
%%%===================================================================

check_commitment(NameAscii, NameSalt, AccountPubKey, Trees, Height) ->
    NSTree = dsdc_trees:ns(Trees),
    CH = dsdns_hash:commitment_hash(NameAscii, NameSalt),
    case dsdns_state_tree:lookup_commitment(CH, NSTree) of
        {value, C} ->
            case dsdns_commitments:owner(C) =:= AccountPubKey of
                true  ->
                    CreatedAt = dsdns_commitments:created(C),
                    Delta = dsdc_governance:name_claim_preclaim_delta(),
                    if CreatedAt + Delta =< Height -> ok;
                       true -> {error, commitment_delta_too_small}
                    end;
                false -> {error, commitment_not_owned}
            end;
        none ->
            {error, name_not_preclaimed}
    end.

check_name(NameAscii, Trees) ->
    NSTree = dsdc_trees:ns(Trees),
    NHash = dsdns_hash:name_hash(NameAscii),
    case dsdns_state_tree:lookup_name(NHash, NSTree) of
        {value, _} ->
            {error, name_already_taken};
        none ->
            ok
    end.


version() ->
    ?NAME_CLAIM_TX_VSN.
