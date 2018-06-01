-module(dsdhttp_logic).

-export([ get_top/0
        , get_top_height/0
        , get_top_hash/0
        , get_header_by_hash/1
        , get_header_by_height/1
        , get_headers_list/3
        , get_block_by_height/1
        , get_block_by_hash/1
        , get_block_latest/0
        , get_block_pending/0
        , get_block_genesis/0
        , get_block_range_by_hash/2
        , get_block_range_by_height/2
        , get_genesis_hash/0
        , get_top_blocks_time_summary/1
        ]).

-export([ get_account_balance/1
        , get_account_balance_at_hash/2
        , get_all_accounts_balances/0
        ]).

-export([ version/0
        , revision/0
        ]).

-export([ contract_compile/2
        , contract_call/4
        , contract_encode_call_data/4
        ]).

-export([ miner_key/0
        , peer_pubkey/0]).

-export([ all_peers/0
        , blocked_peers/0
        ]).

-export([cleanup_genesis/1,
         add_missing_to_genesis_block/1]).

-spec get_top() -> {ok, dsdc_headers:header()}.
get_top() ->
    Header = dsdc_chain:top_header(),
    {ok, Header}.

-spec get_top_height() -> {ok, integer()}.
get_top_height() ->
    TopHeader = dsdc_chain:top_header(),
    Height = dsdc_headers:height(TopHeader),
    {ok, Height}.

-spec get_top_hash() -> {ok, binary()}.
get_top_hash() ->
    TopHeader = dsdc_chain:top_header(),
    {ok, _Hash} = dsdc_headers:hash_header(TopHeader).

-spec get_header_by_hash(binary()) -> {ok, dsdc_headers:header()} | {error, header_not_found}.
get_header_by_hash(Hash) ->
    case dsdc_chain:get_header(Hash) of
        {ok, _Header} = OK -> OK;
        error ->
            {error, header_not_found}
    end.

-spec get_header_by_height(integer()) -> {ok, dsdc_headers:header()} | {error, chain_too_short}.
get_header_by_height(Height) ->
    case dsdc_chain:get_header_by_height(Height) of
        {ok, _Header} = OK -> OK;
        {error, chain_too_short} = Err ->
            Err
    end.

-spec get_block_by_height(integer()) -> {ok, dsdc_blocks:block()} |
                                        {error, block_not_found | chain_too_short}.
get_block_by_height(Height) ->
    case dsdc_chain:get_block_by_height(Height) of
        {ok, Block} ->
            {ok, Block};
        {error, Msg} = Err when Msg =:= block_not_found orelse
                                Msg =:= chain_too_short ->
            Err
    end.

get_block_genesis() ->
    GenBlock = dsdc_chain:genesis_block(),
    {ok, GenBlock}.

get_block_latest() ->
    TopBlock = dsdc_chain:top_block(),
    {ok, TopBlock}.

get_block_pending() ->
    dsdc_conductor:get_block_candidate().

-spec get_block_by_hash(binary()) -> {ok, dsdc_blocks:block()} |
                                     {error, block_not_found}.
get_block_by_hash(Hash) ->
    case dsdc_chain:get_block(Hash) of
        {ok, Block} ->
            {ok, Block};
        error ->
            {error, block_not_found}
    end.

get_block_range_by_hash(HashFrom, HashTo) ->
    dsdc_chain:get_block_range_by_hash(HashFrom, HashTo).

get_block_range_by_height(HeightFrom, HeightTo) ->
    dsdc_chain:get_block_range_by_height(HeightFrom, HeightTo).

-spec get_headers_list(binary(), integer(), backward | forward) ->
    {ok, list()} | {error, atom}.
get_headers_list(Hash, N, Direction) ->
    Fun =
        case Direction of
            forward  -> fun dsdc_chain:get_at_most_n_headers_forward_from_hash/2;
            backward -> fun dsdc_chain:get_n_headers_backwards_from_hash/2
        end,
    case Fun(Hash, N) of
        {ok, _Headers} = OK -> OK;
        error ->
            {error, header_not_found}
    end.

-spec get_account_balance(binary()) -> {ok, integer()}
                                     | {error, account_not_found}.
get_account_balance(Pubkey) when is_binary(Pubkey) ->
    case dsdc_chain:get_account(Pubkey) of
        {value, A} ->
            {ok, dsdc_accounts:balance(A)};
        none ->
            {error, account_not_found}
    end.

get_account_balance_at_hash(AccountPubkey, Hash) ->
    case dsdc_chain:get_account_at_hash(AccountPubkey, Hash) of
        {error, no_state_trees} -> {error, not_on_main_chain};
        none                    -> {error, account_not_found};
        {value, Account}        -> {ok, dsdc_accounts:balance(Account)}
    end.

-spec get_all_accounts_balances() -> {ok, [map()]}.
get_all_accounts_balances() ->
    {ok, AccountsBalances} =
        dsdc_chain:all_accounts_balances_at_hash(dsdc_chain:top_block_hash()),
    FormattedBalances =
        lists:foldl(
          fun({Pubkey, Balance}, Acc) ->
              [#{<<"pub_key">> => Pubkey,
                 <<"balance">> => Balance} | Acc]
          end, [], AccountsBalances),
    {ok, FormattedBalances}.

version() -> {ok, dsdu_info:get_version()}.

revision() -> {ok, dsdu_info:get_revision()}.

get_genesis_hash() -> {ok, dsdc_chain:genesis_hash()}.

get_top_blocks_time_summary(Count) ->
    TimeSummary0 = dsdc_chain:get_top_N_blocks_time_summary(Count),
    TimeSummary =
        lists:foldl(
          fun({Height, Ts, Delta, Difficulty}, Acc) ->
                  [#{height => Height,
                    time => Ts,
                    difficulty => Difficulty,
                    time_delta_to_parent => Delta} | Acc];
            ({Height, Ts, Difficulty}, Acc) ->
                  [#{height => Height,
                    time => Ts,
                    difficulty => Difficulty} | Acc]
          end, [], TimeSummary0),
    {ok, lists:reverse(TimeSummary)}.

contract_compile(Code, Options) ->
    case dsdct_sophia:compile(Code, Options) of
          {ok, _ByteCode} = OK -> OK;
          {error, _ErrorMsg} = Err -> Err
    end.

contract_call(ABI, Code, Function, Argument) ->
    Call =
        fun(CodeOrAddress) ->
          case dsdct_dispatch:call(ABI, CodeOrAddress, Function, Argument) of
              {ok, _Result} = OK -> OK;
              {error, _ErrorMsg} = Err -> Err
          end
        end,
    case ABI of
        <<"sophia-address">> ->
            case dsdc_base58c:safe_decode(contract_pubkey, Code) of
                {ok, ContractAddress} ->
                    Call(ContractAddress);
                _ ->
                    {error, <<"Invalid hash for contract address">>}
            end;
        _ ->
            Call(Code)
    end.

contract_encode_call_data(ABI, Code, Function, Argument) ->
    case dsdct_dispatch:encode_call_data(ABI, Code, Function, Argument) of
        {ok, _Result} = OK -> OK;
        {error, _ErrorMsg} = Err -> Err
    end.

miner_key() ->
    case dsdc_keys:pubkey() of
        {ok, _Pubkey} = OK -> OK;
        {error, key_not_found} = Err -> Err
    end.

peer_pubkey() ->
    case dsdc_keys:peer_pubkey() of
        {ok, _Pubkey} = OK -> OK;
        {error, key_not_found} = Err -> Err
    end.

all_peers() -> dsdc_peers:all().

blocked_peers() -> dsdc_peers:blocked().

%% Private
empty_fields_in_genesis() ->
    [ <<"prev_hash">>,
      <<"pow">>,
      <<"txs_hash">>,
      <<"transactions">>].

% assuming no transactions in genesis block
% if this changes - both functions should be changes:
% empty_fields_in_genesis/0 and values_for_empty_fields_in_genesis/0
values_for_empty_fields_in_genesis() ->
    true = lists:member(<<"transactions">>, empty_fields_in_genesis()),
    #{<<"prev_hash">> => dsdc_base58c:encode(
                           block_hash, dsdc_block_genesis:prev_hash()),
      <<"pow">> => dsdc_headers:serialize_pow_evidence(dsdc_block_genesis:pow()),
      <<"txs_hash">> => dsdc_base58c:encode(
                          block_tx_hash, dsdc_block_genesis:txs_hash()),
      <<"transactions">> => dsdc_block_genesis:transactions()}.

%% to be used for both headers and blocks
cleanup_genesis(#{<<"height">> := 0} = Genesis) ->
    maps:without(empty_fields_in_genesis(), Genesis);
cleanup_genesis(Val) ->
    Val.

add_missing_to_genesis_block(#{<<"height">> := 0} = Block) ->
    maps:merge(Block, values_for_empty_fields_in_genesis());
add_missing_to_genesis_block(Val) ->
    Val.

