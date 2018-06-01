
-module(dsdo_response_tx).

-include("oracle_txs.hrl").

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

%% Additional getters
-export([oracle/1,
         query_id/1,
         response/1]).


-define(ORACLE_RESPONSE_TX_VSN, 1).
-define(ORACLE_RESPONSE_TX_TYPE, oracle_response_tx).
-define(ORACLE_RESPONSE_TX_FEE, 2).

-opaque tx() :: #oracle_response_tx{}.

-export_type([tx/0]).

-spec oracle(tx()) -> dsdc_keys:pubkey().
oracle(#oracle_response_tx{oracle = OraclePubKey}) ->
    OraclePubKey.

-spec query_id(tx()) -> dsdo_query:id().
query_id(#oracle_response_tx{query_id = QId}) ->
    QId.

-spec response(tx()) -> dsdo_query:oracle_response().
response(#oracle_response_tx{response = Response}) ->
    Response.

-spec new(map()) -> {ok, dsdtx:tx()}.
new(#{oracle   := Oracle,
      nonce    := Nonce,
      query_id := QId,
      response := Response,
      fee      := Fee,
      ttl      := TTL}) ->
    Tx = #oracle_response_tx{oracle   = Oracle,
                             nonce    = Nonce,
                             query_id = QId,
                             response = Response,
                             fee      = Fee,
                             ttl      = TTL},
    {ok, dsdtx:new(?MODULE, Tx)}.

-spec type() -> atom().
type() ->
    ?ORACLE_RESPONSE_TX_TYPE.

-spec fee(tx()) -> integer().
fee(#oracle_response_tx{fee = F}) ->
    F.

-spec ttl(tx()) -> dsdc_blocks:height().
ttl(#oracle_response_tx{ttl = TTL}) ->
    TTL.

-spec nonce(tx()) -> non_neg_integer().
nonce(#oracle_response_tx{nonce = Nonce}) ->
    Nonce.

-spec origin(tx()) -> dsdc_keys:pubkey().
origin(#oracle_response_tx{oracle = OraclePubKey}) ->
    OraclePubKey.

%% Oracle should exist, and have enough funds for the fee.
%% QueryId id should match oracle.
-spec check(tx(), dsdtx:tx_context(), dsdc_trees:trees(), dsdc_blocks:height(), non_neg_integer()) ->
        {ok, dsdc_trees:trees()} | {error, term()}.
check(#oracle_response_tx{oracle = OraclePubKey, nonce = Nonce,
                          query_id = QId, fee = Fee}, _Context, Trees, Height, _ConsensusVersion) ->
    case fetch_query(OraclePubKey, QId, Trees) of
        {value, I} ->
            ResponseTTL = dsdo_query:response_ttl(I),
            QueryFee    = dsdo_query:fee(I),
            Checks =
                [fun() -> check_oracle(OraclePubKey, Trees) end,
                 fun() -> check_query(I, OraclePubKey) end,
                 fun() -> dsdtx_utils:check_account(OraclePubKey, Trees,
                                                   Nonce, Fee - QueryFee) end,
                 fun() -> dsdo_utils:check_ttl_fee(Height, ResponseTTL,
                                                  Fee - ?ORACLE_RESPONSE_TX_FEE) end
                ],
            case dsdu_validation:run(Checks) of
                ok              -> {ok, Trees};
                {error, Reason} -> {error, Reason}
            end;
        none -> {error, no_matching_oracle_query}
    end.

-spec accounts(tx()) -> [dsdc_keys:pubkey()].
accounts(#oracle_response_tx{oracle = OraclePubKey}) ->
    [OraclePubKey].

-spec signers(tx(), dsdc_trees:trees()) -> {ok, [dsdc_keys:pubkey()]}.
signers(#oracle_response_tx{oracle = OraclePubKey}, _) ->
    {ok, [OraclePubKey]}.

-spec process(tx(), dsdtx:tx_context(), dsdc_trees:trees(), dsdc_blocks:height(), non_neg_integer()) ->
        {ok, dsdc_trees:trees()}.
process(#oracle_response_tx{oracle = OraclePubKey, nonce = Nonce,
                            query_id = QId, response = Response,
                            fee = Fee}, _Context, Trees0, Height, _ConsensusVersion) ->
    AccountsTree0 = dsdc_trees:accounts(Trees0),
    OraclesTree0  = dsdc_trees:oracles(Trees0),

    Query0 = dsdo_state_tree:get_query(OraclePubKey, QId, OraclesTree0),
    Query1 = dsdo_query:add_response(Height, Response, Query0),
    OraclesTree1 = dsdo_state_tree:enter_query(Query1, OraclesTree0),

    OracleAccount0 = dsdc_accounts_trees:get(OraclePubKey, AccountsTree0),
    {ok, OracleAccount1} = dsdc_accounts:spend(OracleAccount0, Fee, Nonce),
    QueryFee = dsdo_query:fee(Query0),
    {ok, OracleAccount2} = dsdc_accounts:earn(OracleAccount1, QueryFee),
    AccountsTree1 = dsdc_accounts_trees:enter(OracleAccount2, AccountsTree0),

    Trees1 = dsdc_trees:set_accounts(Trees0, AccountsTree1),
    Trees2 = dsdc_trees:set_oracles(Trees1, OraclesTree1),

    {ok, Trees2}.

serialize(#oracle_response_tx{oracle   = OraclePubKey,
                              nonce    = Nonce,
                              query_id = QId,
                              response = Response,
                              fee      = Fee,
                              ttl      = TTL}) ->
    {version(),
    [ {oracle, OraclePubKey}
    , {nonce, Nonce}
    , {query_id, QId}
    , {response, Response}
    , {fee, Fee}
    , {ttl, TTL}
    ]}.

deserialize(?ORACLE_RESPONSE_TX_VSN,
            [ {oracle, OraclePubKey}
            , {nonce, Nonce}
            , {query_id, QId}
            , {response, Response}
            , {fee, Fee}
            , {ttl, TTL}]) ->
    #oracle_response_tx{oracle   = OraclePubKey,
                        nonce    = Nonce,
                        query_id = QId,
                        response = Response,
                        fee      = Fee,
                        ttl      = TTL}.

serialization_template(?ORACLE_RESPONSE_TX_VSN) ->
    [ {oracle, binary}
    , {nonce, int}
    , {query_id, binary}
    , {response, binary}
    , {fee, int}
    , {ttl, int}
    ].

-spec version() -> non_neg_integer().
version() ->
    ?ORACLE_RESPONSE_TX_VSN.

for_client(#oracle_response_tx{ oracle   = OraclePubKey,
                                nonce    = Nonce,
                                query_id = QId,
                                response = Response,
                                fee      = Fee,
                                ttl      = TTL}) ->
    #{<<"data_schema">> => <<"OracleResponseTxJSON">>, % swagger schema name
      <<"vsn">> => version(),
      <<"oracle">> => dsdc_base58c:encode(oracle_pubkey, OraclePubKey),
      <<"nonce">> => Nonce,
      <<"query_id">> => dsdc_base58c:encode(oracle_query_id, QId),
      <<"response">> => Response,
      <<"fee">> => Fee,
      <<"ttl">> => TTL}.

%% -- Local functions  -------------------------------------------------------

fetch_query(OId, QId, Trees) ->
    OraclesTree  = dsdc_trees:oracles(Trees),
    dsdo_state_tree:lookup_query(OId, QId, OraclesTree).

check_query(I, OraclePubKey) ->
    case OraclePubKey == dsdo_query:oracle_address(I) of
        true  ->
            case dsdo_query:is_closed(I) of
                true  -> {error, oracle_closed_for_response};
                false -> ok
            end;
        false -> {error, oracle_does_not_match_query_id}
    end.

check_oracle(OraclePubKey, Trees) ->
    OraclesTree  = dsdc_trees:oracles(Trees),
    case dsdo_state_tree:lookup_oracle(OraclePubKey, OraclesTree) of
        {value, _Oracle} -> ok;
        none -> {error, oracle_does_not_exist}
    end.
