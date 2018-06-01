
-module(dsdc_trees).

-include("blocks.hrl").

%% API

-export([accounts/1,
         commit_to_db/1,
         hash/1,
         new/0,
         channels/1,
         ns/1,
         oracles/1,
         calls/1,
         contracts/1,
         set_accounts/2,
         set_calls/2,
         set_channels/2,
         set_contracts/2,
         set_ns/2,
         set_oracles/2
        ]).

-export([ensure_account/2]).

-export([apply_txs_on_state_trees/4,
         apply_txs_on_state_trees_strict/4,
         grant_fee_to_miner/3,
         perform_pre_transformations/2
        ]).

%% Proof of inclusion
-export([add_poi/4,
         deserialize_poi/1,
         new_poi/1,
         poi_hash/1,
         serialize_poi/1,
         verify_poi/4
        ]).

-record(trees, {
          accounts  :: dsdc_accounts_trees:tree(),
          calls     :: dsdct_call_state_tree:tree(),
          channels  :: dsdsc_state_tree:tree(),
          contracts :: dsdct_state_tree:tree(),
          ns        :: dsdns_state_tree:tree(),
          oracles   :: dsdo_state_tree:tree()}).

-record(poi, {
          accounts  :: part_poi(),
          calls     :: part_poi(),
          channels  :: part_poi(),
          contracts :: part_poi(),
          ns        :: part_poi(),
          oracles   :: part_poi()
         }).

-opaque trees() :: #trees{}.
-opaque poi() :: #poi{}.

-type part_poi() :: 'empty' | {'poi', dsdc_poi:poi()}.
-type tree_type() :: 'accounts'
                   | 'calls'
                   | 'channels'
                   | 'contracts'
                   | 'ns'
                   | 'oracles'.

-export_type([ trees/0
             , poi/0
             ]).

%%%%=============================================================================
%% API
%%%=============================================================================

-spec new() -> trees().
new() ->
    #trees{accounts  = dsdc_accounts_trees:empty_with_backend(),
           calls     = dsdct_call_state_tree:empty_with_backend(),
           channels  = dsdsc_state_tree:empty_with_backend(),
           contracts = dsdct_state_tree:empty_with_backend(),
           ns        = dsdns_state_tree:empty_with_backend(),
           oracles   = dsdo_state_tree:empty_with_backend()
          }.

-spec new_poi(trees()) -> poi().
new_poi(Trees) ->
    internal_new_poi(Trees).

-spec add_poi(tree_type(), dsdc_keys:pubkey(), trees(), poi()) -> {'ok', binary(), poi()}
                                                      | {'error', term()}.
add_poi(accounts, PubKey, Trees, #poi{} = Poi) ->
    internal_add_accounts_poi(PubKey, accounts(Trees), Poi);
add_poi(Type,_PubKey,_Trees, #poi{} =_Poi) ->
    error({nyi, Type}).

-spec poi_hash(poi()) -> state_hash().
poi_hash(#poi{} = Poi) ->
    internal_poi_hash(Poi).

-spec serialize_poi(poi()) -> binary().
serialize_poi(#poi{} = Poi) ->
    internal_serialize_poi(Poi).

-spec deserialize_poi(binary()) -> poi().
deserialize_poi(Bin) when is_binary(Bin) ->
    internal_deserialize_poi(Bin).

-spec verify_poi(tree_type(), dsdc_keys:pubkey(), binary(), poi()) -> 'ok'
                                                          | {'error', term()}.
verify_poi(accounts, PubKey, SerializedAccount, #poi{} = Poi) ->
    internal_verify_accounts_poi(PubKey, SerializedAccount, Poi);
verify_poi(Type,_PubKey,_Account, #poi{} =_Poi) ->
    error({nyi, Type}).

-spec commit_to_db(trees()) -> trees().
commit_to_db(Trees) ->
    %% Make this in a transaction to get atomicity.
    dsdc_db:transaction(fun() -> internal_commit_to_db(Trees) end).

hash(Trees) ->
    internal_hash(Trees).

-spec accounts(trees()) -> dsdc_accounts_trees:tree().
accounts(Trees) ->
    Trees#trees.accounts.

-spec set_accounts(trees(), dsdc_accounts_trees:tree()) -> trees().
set_accounts(Trees, Accounts) ->
    Trees#trees{accounts = Accounts}.

-spec channels(trees()) -> dsdsc_state_tree:tree().
channels(Trees) ->
    Trees#trees.channels.

-spec set_channels(trees(), dsdsc_state_tree:tree()) -> trees().
set_channels(Trees, Channels) ->
    Trees#trees{channels = Channels}.

-spec ns(trees()) -> dsdns_state_tree:tree().
ns(Trees) ->
    Trees#trees.ns.

-spec set_ns(trees(), dsdns_state_tree:tree()) -> trees().
set_ns(Trees, Names) ->
    Trees#trees{ns = Names}.

-spec oracles(trees()) -> dsdo_state_tree:tree().
oracles(Trees) ->
    Trees#trees.oracles.

-spec set_oracles(trees(), dsdo_state_tree:tree()) -> trees().
set_oracles(Trees, Oracles) ->
    Trees#trees{oracles = Oracles}.

-spec perform_pre_transformations(trees(), dsdc_blocks:height()) -> trees().
perform_pre_transformations(Trees, Height) ->
    Trees0 = dsdct_call_state_tree:prune(Height, Trees),
    Trees1 = dsdo_state_tree:prune(Height, Trees0),
    set_ns(Trees1, dsdns_state_tree:prune(Height, ns(Trees1))).

-spec calls(trees()) -> dsdct_call_state_tree:tree().
calls(Trees) ->
    Trees#trees.calls.

-spec set_calls(trees(), dsdct_call_state_tree:tree()) -> trees().
set_calls(Trees, Calls) ->
    Trees#trees{calls = Calls}.

-spec contracts(trees()) -> dsdct_state_tree:tree().
contracts(Trees) ->
    Trees#trees.contracts.

-spec set_contracts(trees(), dsdct_state_tree:tree()) -> trees().
set_contracts(Trees, Contracts) ->
    Trees#trees{contracts = Contracts}.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

internal_hash(Trees) ->
    %% Note that all hash sizes are checked in pad_empty/2
    Bin = <<?PROTOCOL_VERSION:64,
            (accounts_hash(Trees))  /binary,
            (calls_hash(Trees))     /binary,
            (channels_hash(Trees))  /binary,
            (contracts_hash(Trees)) /binary,
            (ns_hash(Trees))        /binary,
            (oracles_hash(Trees))   /binary
          >>,
    dsdc_hash:hash(state_trees, Bin).

accounts_hash(Trees) ->
    pad_empty(dsdc_accounts_trees:root_hash(accounts(Trees))).

calls_hash(Trees) ->
    pad_empty(dsdct_call_state_tree:root_hash(calls(Trees))).

channels_hash(Trees) ->
    pad_empty(dsdsc_state_tree:root_hash(channels(Trees))).

contracts_hash(Trees) ->
    pad_empty(dsdct_state_tree:root_hash(contracts(Trees))).

oracles_hash(Trees) ->
    pad_empty(dsdo_state_tree:root_hash(oracles(Trees))).

ns_hash(Trees) -> pad_empty(dsdns_state_tree:root_hash(ns(Trees))).

pad_empty({ok, H}) when is_binary(H), byte_size(H) =:= ?STATE_HASH_BYTES -> H;
pad_empty({error, empty}) -> <<0:?STATE_HASH_BYTES/unit:8>>.

internal_commit_to_db(Trees) ->
    Trees#trees{ contracts = dsdct_state_tree:commit_to_db(contracts(Trees))
               , calls     = dsdct_call_state_tree:commit_to_db(calls(Trees))
               , channels  = dsdsc_state_tree:commit_to_db(channels(Trees))
               , ns        = dsdns_state_tree:commit_to_db(ns(Trees))
               , oracles   = dsdo_state_tree:commit_to_db(oracles(Trees))
               , accounts  = dsdc_accounts_trees:commit_to_db(accounts(Trees))
               }.

apply_txs_on_state_trees(SignedTxs, Trees, Height, ConsensusVersion) ->
    apply_txs_on_state_trees(SignedTxs, Trees, Height, ConsensusVersion, false).

apply_txs_on_state_trees_strict(SignedTxs, Trees, Height, ConsensusVersion) ->
    apply_txs_on_state_trees(SignedTxs, Trees, Height, ConsensusVersion, true).

apply_txs_on_state_trees(SignedTxs, Trees, Height, ConsensusVersion, Strict) ->
    apply_txs_on_state_trees(SignedTxs, [], Trees, Height, ConsensusVersion, Strict).

apply_txs_on_state_trees([], FilteredSignedTxs, Trees, _Height,_ConsensusVersion,_Strict) ->
    {ok, lists:reverse(FilteredSignedTxs), Trees};
apply_txs_on_state_trees([SignedTx | Rest], FilteredSignedTxs, Trees0, Height, ConsensusVersion, Strict) ->
    case dsdtx_sign:verify(SignedTx, Trees0) of
        ok ->
            Tx = dsdtx_sign:tx(SignedTx),
            case dsdtx:check(Tx, Trees0, Height, ConsensusVersion) of
                {ok, Trees1} ->
                    {ok, Trees2} = dsdtx:process(Tx, Trees1, Height, ConsensusVersion),
                    apply_txs_on_state_trees(Rest, [SignedTx | FilteredSignedTxs], Trees2, Height, ConsensusVersion, Strict);
                {error, Reason} when Strict ->
                    lager:debug("Tx ~p cannot be applied due to an error ~p", [Tx, Reason]),
                    {error, Reason};
                {error, Reason} when not Strict ->
                    lager:debug("Tx ~p cannot be applied due to an error ~p", [Tx, Reason]),
                    apply_txs_on_state_trees(Rest, FilteredSignedTxs, Trees0, Height, ConsensusVersion, Strict)
            end;
        {error, signature_check_failed} = E when Strict ->
            lager:debug("Signed tx ~p is not correctly signed.", [SignedTx]),
            E;
        {error, signature_check_failed} when not Strict ->
            lager:debug("Signed tx ~p is not correctly signed.", [SignedTx]),
            apply_txs_on_state_trees(Rest, FilteredSignedTxs, Trees0, Height, ConsensusVersion, Strict)
    end.

-spec grant_fee_to_miner(dsdc_keys:pubkey(), trees(), non_neg_integer()) ->
                                trees().
grant_fee_to_miner(MinerPubkey, Trees0, TotalFee) ->
    Trees1 = ensure_account(MinerPubkey, Trees0),
    AccountsTrees1 = accounts(Trees1),

    {value, Account1} = dsdc_accounts_trees:lookup(MinerPubkey, AccountsTrees1),
    {ok, Account} = dsdc_accounts:earn(Account1, TotalFee),

    AccountsTrees = dsdc_accounts_trees:enter(Account, AccountsTrees1),
    set_accounts(Trees1, AccountsTrees).

-spec ensure_account(dsdc_keys:pubkey(), trees()) -> trees().
ensure_account(AccountPubkey, Trees0) ->
    AccountsTrees0 = dsdc_trees:accounts(Trees0),
    case dsdc_accounts_trees:lookup(AccountPubkey, AccountsTrees0) of
        {value, _Account} ->
            Trees0;
        none ->
            %% Add newly referenced account (w/0 amount) to the state
            Account = dsdc_accounts:new(AccountPubkey, 0),
            AccountsTrees = dsdc_accounts_trees:enter(Account, AccountsTrees0),
            dsdc_trees:set_accounts(Trees0, AccountsTrees)
    end.

%%%=============================================================================
%%% Proof of Inclusion (PoI)
%%%=============================================================================

internal_new_poi(Trees) ->
    #poi{ accounts  = new_part_poi(accounts_hash(Trees))
        , calls     = new_part_poi(calls_hash(Trees))
        , channels  = new_part_poi(channels_hash(Trees))
        , contracts = new_part_poi(contracts_hash(Trees))
        , ns        = new_part_poi(ns_hash(Trees))
        , oracles   = new_part_poi(oracles_hash(Trees))
        }.

internal_poi_hash(#poi{} = POI) ->
    %% Note that all hash sizes are checked in pad_empty/2
    Bin = <<?PROTOCOL_VERSION:64,
            (part_poi_hash(POI#poi.accounts)) /binary,
            (part_poi_hash(POI#poi.calls))    /binary,
            (part_poi_hash(POI#poi.channels)) /binary,
            (part_poi_hash(POI#poi.contracts))/binary,
            (part_poi_hash(POI#poi.ns))       /binary,
            (part_poi_hash(POI#poi.oracles))  /binary
          >>,
    dsdc_hash:hash(state_trees, Bin).

part_poi_hash(empty) -> <<0:?STATE_HASH_BYTES/unit:8>>;
part_poi_hash({poi, Poi}) -> dsdc_poi:root_hash(Poi).

internal_add_accounts_poi(_Pubkey,_Trees, #poi{accounts = empty}) ->
    {error, not_present};
internal_add_accounts_poi(Pubkey, Trees, #poi{accounts = {poi, APoi}} = Poi) ->
    case dsdc_accounts_trees:add_poi(Pubkey, Trees, APoi) of
        {ok, SerializedAccount, NewAPoi} ->
            {ok, SerializedAccount, Poi#poi{accounts = {poi, NewAPoi}}};
        {error, _} = E -> E
    end.

internal_verify_accounts_poi(_AccountPubkey,_Account, #poi{accounts = empty}) ->
    {error, empty_accounts_poi};
internal_verify_accounts_poi(AccountPubkey, Account, #poi{accounts = {poi, APoi}}) ->
    dsdc_accounts_trees:verify_poi(AccountPubkey, Account, APoi).

new_part_poi(<<0:?STATE_HASH_BYTES/unit:8>>) ->
    empty;
new_part_poi(<<_:?STATE_HASH_BYTES/unit:8>> = Hash) ->
    {poi, dsdc_poi:new(Hash)}.

-define(POI_VSN, 1).

internal_serialize_poi(#poi{ accounts  = Accounts
                           , calls     = Calls
                           , channels  = Channels
                           , contracts = Contracts
                           , ns        = Ns
                           , oracles   = Oracles
                           }) ->

    Fields = [ {accounts  , serialization_format(Accounts)}
             , {calls     , serialization_format(Calls)}
             , {channels  , serialization_format(Channels)}
             , {contracts , serialization_format(Contracts)}
             , {ns        , serialization_format(Ns)}
             , {oracles   , serialization_format(Oracles)}
             ],
    dsdc_object_serialization:serialize(trees_poi,
                                       ?POI_VSN,
                                       internal_serialize_poi_template(?POI_VSN),
                                       Fields).

serialization_format(empty) -> [];
serialization_format({poi, Poi}) -> [dsdc_poi:serialization_format(Poi)].

from_serialization_format([]) -> empty;
from_serialization_format([Poi]) -> {poi, dsdc_poi:from_serialization_format(Poi)}.

internal_deserialize_poi(Bin) ->
    Template = internal_serialize_poi_template(?POI_VSN),

    [ {accounts  , Accounts}
    , {calls     , Calls}
    , {channels  , Channels}
    , {contracts , Contracts}
    , {ns        , Ns}
    , {oracles   , Oracles}
    ] = dsdc_object_serialization:deserialize(trees_poi, ?POI_VSN, Template, Bin),

    #poi{ accounts  = from_serialization_format(Accounts)
        , calls     = from_serialization_format(Calls)
        , channels  = from_serialization_format(Channels)
        , contracts = from_serialization_format(Contracts)
        , ns        = from_serialization_format(Ns)
        , oracles   = from_serialization_format(Oracles)
        }.


internal_serialize_poi_template(?POI_VSN) ->
    PoiTemplate = dsdc_poi:serialization_format_template(),
    [{X, [PoiTemplate]}
     || X <- [ accounts
             , calls
             , channels
             , contracts
             , ns
             , oracles
             ]].
