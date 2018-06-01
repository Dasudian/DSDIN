
-module(dsdo_query_tx).

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
         query/1,
         query_fee/1,
         query_ttl/1,
         response_ttl/1,
         sender/1]).


-define(ORACLE_QUERY_TX_VSN, 1).
-define(ORACLE_QUERY_TX_TYPE, oracle_query_tx).
-define(ORACLE_QUERY_TX_FEE, 2).

-opaque tx() :: #oracle_query_tx{}.

-export_type([tx/0]).

-spec sender(tx()) -> dsdc_keys:pubkey().
sender(#oracle_query_tx{sender = SenderPubKey}) ->
    SenderPubKey.

-spec oracle(tx()) -> dsdc_keys:pubkey().
oracle(#oracle_query_tx{oracle = OraclePubKey}) ->
    OraclePubKey.

-spec query(tx()) -> dsdo_oracles:query().
query(#oracle_query_tx{query = Query}) ->
    Query.

-spec query_fee(tx()) -> integer().
query_fee(#oracle_query_tx{query_fee = QueryFee}) ->
    QueryFee.

-spec query_ttl(tx()) -> dsdo_oracles:ttl().
query_ttl(#oracle_query_tx{query_ttl = QueryTTL}) ->
    QueryTTL.

-spec response_ttl(tx()) -> dsdo_oracles:relative_ttl().
response_ttl(#oracle_query_tx{response_ttl = ResponseTTL}) ->
    ResponseTTL.

-spec new(map()) -> {ok, dsdtx:tx()}.
new(#{sender        := SenderPubKey,
      nonce         := Nonce,
      oracle        := Oracle,
      query         := Query,
      query_fee     := QueryFee,
      query_ttl     := QueryTTL,
      response_ttl  := ResponseTTL,
      fee           := Fee,
      ttl           := TTL}) ->
    Tx = #oracle_query_tx{sender        = SenderPubKey,
                          nonce         = Nonce,
                          oracle        = Oracle,
                          query         = Query,
                          query_fee     = QueryFee,
                          query_ttl     = QueryTTL,
                          response_ttl  = ResponseTTL,
                          fee           = Fee,
                          ttl           = TTL},
    {ok, dsdtx:new(?MODULE, Tx)}.

-spec type() -> atom().
type() ->
    ?ORACLE_QUERY_TX_TYPE.

-spec fee(tx()) -> integer().
fee(#oracle_query_tx{fee = F}) ->
    F.

-spec ttl(tx()) -> dsdc_blocks:height().
ttl(#oracle_query_tx{ttl = TTL}) ->
    TTL.

-spec nonce(tx()) -> non_neg_integer().
nonce(#oracle_query_tx{nonce = Nonce}) ->
    Nonce.

-spec origin(tx()) -> dsdc_keys:pubkey().
origin(#oracle_query_tx{sender = SenderPubKey}) ->
    SenderPubKey.

%% SenderAccount should exist, and have enough funds for the fee + the query_fee.
%% Oracle should exist, and query_fee should be enough
%% Fee should cover TTL
-spec check(tx(), dsdtx:tx_context(), dsdc_trees:trees(), dsdc_blocks:height(), non_neg_integer()) ->
        {ok, dsdc_trees:trees()} | {error, term()}.
check(#oracle_query_tx{sender = SenderPubKey, nonce = Nonce,
                       oracle = OraclePubKey, query_fee = QFee,
                       query_ttl = QTTL, response_ttl = RTTL,
                       fee = Fee} = QTx, _Context, Trees, Height, _ConsensusVersion) ->
    Checks =
        [fun() -> dsdtx_utils:check_account(SenderPubKey, Trees, Nonce, Fee + QFee) end,
         fun() -> dsdo_utils:check_ttl_fee(Height, QTTL, Fee - ?ORACLE_QUERY_TX_FEE) end,
         fun() -> check_oracle(OraclePubKey, Trees, QFee, Height, QTTL, RTTL) end,
         fun() -> check_query(QTx, Trees, Height) end
        ],

    case dsdu_validation:run(Checks) of
        ok              -> {ok, Trees};
        {error, Reason} -> {error, Reason}
    end.

-spec accounts(tx()) -> [dsdc_keys:pubkey()].
accounts(#oracle_query_tx{sender = SenderPubKey,
                          oracle = OraclePubKey}) ->
    [SenderPubKey, OraclePubKey].

-spec signers(tx(), dsdc_trees:trees()) -> {ok, [dsdc_keys:pubkey()]}.
signers(#oracle_query_tx{sender = SenderPubKey}, _) ->
    {ok, [SenderPubKey]}.

-spec process(tx(), dsdtx:tx_context(), dsdc_trees:trees(), dsdc_blocks:height(), non_neg_integer()) ->
        {ok, dsdc_trees:trees()}.
process(#oracle_query_tx{sender = SenderPubKey, nonce = Nonce, fee = Fee,
                         query_fee = QueryFee} = QueryTx, _Context, Trees0, Height, _ConsensusVersion) ->
    AccountsTree0 = dsdc_trees:accounts(Trees0),
    OraclesTree0  = dsdc_trees:oracles(Trees0),

    Sender0 = dsdc_accounts_trees:get(SenderPubKey, AccountsTree0),
    {ok, Sender1} = dsdc_accounts:spend(Sender0, QueryFee + Fee, Nonce),
    AccountsTree1 = dsdc_accounts_trees:enter(Sender1, AccountsTree0),

    Query = dsdo_query:new(QueryTx, Height),
    OraclesTree1 = dsdo_state_tree:insert_query(Query, OraclesTree0),

    Trees1 = dsdc_trees:set_accounts(Trees0, AccountsTree1),
    Trees2 = dsdc_trees:set_oracles(Trees1, OraclesTree1),

    {ok, Trees2}.

serialize(#oracle_query_tx{sender        = SenderPubKey,
                           nonce         = Nonce,
                           oracle        = OraclePubKey,
                           query         = Query,
                           query_fee     = QueryFee,
                           query_ttl     = {QueryTTLType0, QueryTTLValue},
                           response_ttl  = {?ttl_delta_atom, ResponseTTLValue},
                           fee           = Fee,
                           ttl           = TTL}) ->
    QueryTTLType = case QueryTTLType0 of
                       ?ttl_delta_atom -> ?ttl_delta_int;
                       ?ttl_block_atom -> ?ttl_block_int
                   end,
    {version(),
     [ {sender, SenderPubKey}
     , {nonce, Nonce}
     , {oracle, OraclePubKey}
     , {query, Query}
     , {query_fee, QueryFee}
     , {query_ttl_type, QueryTTLType}
     , {query_ttl_value, QueryTTLValue}
     , {response_ttl_type, ?ttl_delta_int}
     , {response_ttl_value, ResponseTTLValue}
     , {fee, Fee}
     , {ttl, TTL}
     ]}.

deserialize(?ORACLE_QUERY_TX_VSN,
            [ {sender, SenderPubKey}
            , {nonce, Nonce}
            , {oracle, OraclePubKey}
            , {query, Query}
            , {query_fee, QueryFee}
            , {query_ttl_type, QueryTTLType0}
            , {query_ttl_value, QueryTTLValue}
            , {response_ttl_type, ?ttl_delta_int}
            , {response_ttl_value, ResponseTTLValue}
            , {fee, Fee}
            , {ttl, TTL}]) ->
    QueryTTLType = case QueryTTLType0 of
                       ?ttl_delta_int -> ?ttl_delta_atom;
                       ?ttl_block_int -> ?ttl_block_atom
                   end,
    #oracle_query_tx{sender        = SenderPubKey,
                     nonce         = Nonce,
                     oracle        = OraclePubKey,
                     query         = Query,
                     query_fee     = QueryFee,
                     query_ttl     = {QueryTTLType, QueryTTLValue},
                     response_ttl  = {?ttl_delta_atom, ResponseTTLValue},
                     fee           = Fee,
                     ttl           = TTL}.

serialization_template(?ORACLE_QUERY_TX_VSN) ->
    [ {sender, binary}
    , {nonce, int}
    , {oracle, binary}
    , {query, binary}
    , {query_fee, int}
    , {query_ttl_type, int}
    , {query_ttl_value, int}
    , {response_ttl_type, int}
    , {response_ttl_value, int}
    , {fee, int}
    , {ttl, int}
    ].

-spec version() -> non_neg_integer().
version() ->
    ?ORACLE_QUERY_TX_VSN.

for_client(#oracle_query_tx{sender        = SenderPubKey,
                            nonce         = Nonce,
                            oracle        = OraclePubKey,
                            query         = Query,
                            query_fee     = QueryFee,
                            query_ttl     = {QueryTLLType, QueryTTLValue},
                            response_ttl  = {delta=ResponseTTLType, ResponseTTLValue},
                            fee           = Fee,
                            ttl           = TTL}) ->
    #{<<"data_schema">> => <<"OracleQueryTxJSON">>, % swagger schema name
      <<"vsn">> => version(),
      <<"sender">> => dsdc_base58c:encode(account_pubkey, SenderPubKey),
      <<"nonce">> => Nonce,
      <<"oracle">> => dsdc_base58c:encode(oracle_pubkey, OraclePubKey),
      <<"query">> => Query,
      <<"query_fee">> => QueryFee,
      <<"query_ttl">> => #{<<"type">> => QueryTLLType,
                           <<"value">> => QueryTTLValue},
      <<"response_ttl">> => #{<<"type">> => ResponseTTLType,
                              <<"value">> => ResponseTTLValue},
      <<"fee">> => Fee,
      <<"ttl">> => TTL}.

%% -- Local functions  -------------------------------------------------------

check_query(Q, Trees, Height) ->
    Oracles  = dsdc_trees:oracles(Trees),
    I        = dsdo_query:new(Q, Height),
    OracleId = dsdo_query:oracle_address(I),
    Id       = dsdo_query:id(I),
    case dsdo_state_tree:lookup_query(OracleId, Id, Oracles) of
        none       -> ok;
        {value, _} -> {error, oracle_query_already_present}
    end.

check_oracle(OraclePubKey, Trees, QueryFee, Height, QTTL, RTTL) ->
    OraclesTree  = dsdc_trees:oracles(Trees),
    case dsdo_state_tree:lookup_oracle(OraclePubKey, OraclesTree) of
        {value, Oracle} ->
            case QueryFee >= dsdo_oracles:query_fee(Oracle) of
                true  -> check_oracle_ttl(Oracle, Height, QTTL, RTTL);
                false -> {error, query_fee_too_low}
            end;
        none -> {error, oracle_does_not_exist}
    end.

check_oracle_ttl(O, Height, QTTL, RTTL) ->
    try
        Delta  = dsdo_utils:ttl_delta(Height, QTTL),
        MaxTTL = dsdo_utils:ttl_expiry(Height + Delta, RTTL),
        case dsdo_oracles:expires(O) < MaxTTL of
            false -> ok;
            true  -> {error, too_long_ttl}
        end
    catch _:_ ->
        {error, invalid_ttl}
    end.
