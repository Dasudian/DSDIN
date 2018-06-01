
-module(dsdc_block_candidate).

-export([ apply_block_txs/5
        , apply_block_txs_strict/5
        , calculate_fee/1
        , calculate_total_fee/1
        , create/1
        , create_with_state/4
        ]).

-export([adjust_target/2]).

%% -- API functions ----------------------------------------------------------
-spec create(dsdc_blocks:block() | dsdc_blocks:block_header_hash()) ->
        {ok, dsdc_blocks:block(), term()} | {error, term()}.
create(BlockHash) when is_binary(BlockHash) ->
    case dsdc_chain:get_block(BlockHash) of
        {ok, Block} ->
            int_create(BlockHash, Block);
        error ->
            {error, block_not_found}
    end;
create(Block) ->
    {ok, BlockHash} = dsdc_blocks:hash_internal_representation(Block),
    int_create(BlockHash, Block).

-spec apply_block_txs(list(dsdtx_sign:signed_tx()), dsdc_keys:pubkey(), dsdc_trees:trees(),
                      dsdc_blocks:height(), non_neg_integer()) ->
        {ok, list(dsdtx_sign:signed_tx()), dsdc_trees:trees()}.
apply_block_txs(Txs, Miner, Trees, Height, Version) ->
    {ok, Txs1, Trees1, _} = int_apply_block_txs(Txs, Miner, Trees, Height, Version, false),
    {ok, Txs1, Trees1}.

-spec apply_block_txs_strict(list(dsdtx_sign:signed_tx()), dsdc_keys:pubkey(),
                             dsdc_trees:trees(), dsdc_blocks:height(), non_neg_integer()) ->
        {ok, list(dsdtx_sign:signed_tx()), dsdc_trees:trees()} | {error, term()}.
apply_block_txs_strict(Txs, Miner, Trees, Height, Version) ->
    case int_apply_block_txs(Txs, Miner, Trees, Height, Version, true) of
        Err = {error, _}      -> Err;
        {ok, Txs1, Trees1, _} -> {ok, Txs1, Trees1}
    end.

-spec create_with_state(dsdc_blocks:block(), dsdc_keys:pubkey(),
                        list(dsdtx_sign:signed_tx()), dsdc_trees:trees()) ->
        {dsdc_blocks:block(), dsdc_trees:trees()}.
create_with_state(Block, Miner, Txs, Trees) ->
    {ok, BlockHash} = dsdc_blocks:hash_internal_representation(Block),
    {ok, NewBlock, #{ trees := NewTrees}} =
        int_create_block(BlockHash, Block, Trees, Miner, Txs),
    {NewBlock, NewTrees}.

-spec adjust_target(dsdc_blocks:block(), list(dsdc_headers:header())) ->
       {ok, dsdc_blocks:block()} | {error, term()}.
adjust_target(Block, AdjHeaders) ->
    Header = dsdc_blocks:to_header(Block),
    DeltaHeight = dsdc_governance:blocks_to_check_difficulty_count(),
    case dsdc_headers:height(Header) =< DeltaHeight of
        true ->
            %% For the first DeltaHeight blocks, use pre-defined target
            {ok, Block};
        false when DeltaHeight == length(AdjHeaders) ->
            CalculatedTarget = dsdc_target:recalculate(Header, AdjHeaders),
            Block1 = dsdc_blocks:set_target(Block, CalculatedTarget),
            {ok, Block1};
        false -> %% Wrong number of headers in AdjHeaders...
            {error, {wrong_headers_for_target_adjustment, DeltaHeight, length(AdjHeaders)}}
    end.

-spec calculate_fee(list(dsdtx_sign:signed_tx())) -> non_neg_integer().
calculate_fee(SignedTxs) ->
    lists:foldl(
        fun(SignedTx, TotalFee) ->
            Fee = dsdtx:fee(dsdtx_sign:tx(SignedTx)),
            TotalFee + Fee
        end, 0, SignedTxs).

-spec calculate_total_fee(list(dsdtx_sign:signed_tx())) -> non_neg_integer().
calculate_total_fee(SignedTxs) ->
    TxsFee = calculate_fee(SignedTxs),
    dsdc_governance:block_mine_reward() + TxsFee.

%% -- Internal functions -----------------------------------------------------
int_create(BlockHash, Block) ->
    case dsdc_chain:get_block_state(BlockHash) of
        {ok, Trees} ->
            int_create(BlockHash, Block, Trees);
        error ->
            {error, block_state_not_found}
    end.

int_create(BlockHash, Block, Trees) ->
    N = dsdc_governance:blocks_to_check_difficulty_count(),
    case dsdc_blocks:height(Block) < N of
        true  ->
            int_create(BlockHash, Block, Trees, []);
        false ->
            case dsdc_chain:get_n_headers_backwards_from_hash(BlockHash, N) of
                {ok, Headers} ->
                    int_create(BlockHash, Block, Trees, Headers);
                error ->
                    {error, headers_for_target_adjustment_not_found}
            end
    end.

int_create(BlockHash, Block, Trees, AdjChain) ->
    MaxN = dsdc_governance:max_txs_in_block(),
    {ok, Txs} = dsdc_tx_pool:get_candidate(MaxN, BlockHash),
    case dsdc_keys:pubkey() of
        {ok, Miner} ->
            int_create(BlockHash, Block, Trees, Miner, Txs, AdjChain);
        {error, _} = Error ->
            Error
    end.

int_create(PrevBlockHash, PrevBlock, Trees, Miner, Txs, AdjChain) ->
    {ok, Block, BlockInfo} = int_create_block(PrevBlockHash, PrevBlock, Trees, Miner, Txs),
    case adjust_target(Block, AdjChain) of
        {ok, AdjBlock} -> {ok, AdjBlock, BlockInfo#{ adj_chain => AdjChain }};
        {error, _}     -> {error, failed_to_adjust_target}
    end.

int_create_block(PrevBlockHash, PrevBlock, Trees, Miner, Txs) ->
    PrevBlockHeight = dsdc_blocks:height(PrevBlock),

    %% Assert correctness of last block protocol version, as minimum
    %% sanity check on previous block and state (mainly for potential
    %% stale state persisted in DB and for development testing).
    ExpectedPrevBlockVersion =
        dsdc_hard_forks:protocol_effective_at_height(PrevBlockHeight),
    {ExpectedPrevBlockVersion, _} = {dsdc_blocks:version(PrevBlock),
                                     {expected, ExpectedPrevBlockVersion}},

    Height = PrevBlockHeight + 1,
    Version = dsdc_hard_forks:protocol_effective_at_height(Height),

    {ok, Txs1, Trees2, TotFee} =
        int_apply_block_txs(Txs, Miner, Trees, Height, Version, false),

    TxsTree = dsdc_txs_trees:from_txs(Txs1),
    TxsRootHash = dsdc_txs_trees:pad_empty(dsdc_txs_trees:root_hash(TxsTree)),

    NewBlock = dsdc_blocks:new(Height, PrevBlockHash, dsdc_trees:hash(Trees2),
                              TxsRootHash, Txs1, dsdc_blocks:target(PrevBlock),
                              0, dsdu_time:now_in_msecs(), Version, Miner),

    BlockInfo = #{ trees => Trees2, tot_fee => TotFee, txs_tree => TxsTree },
    {ok, NewBlock, BlockInfo}.

%% Non-strict
int_apply_block_txs(Txs, Miner, Trees, Height, Version, false) ->
    Trees0 = dsdc_trees:perform_pre_transformations(Trees, Height),
    {ok, Txs1, Trees1} =
        dsdc_trees:apply_txs_on_state_trees(Txs, Trees0, Height, Version),
    TotFee = calculate_total_fee(Txs1),
    Trees2 = dsdc_trees:grant_fee_to_miner(Miner, Trees1, TotFee),
    {ok, Txs1, Trees2, TotFee};
%% strict
int_apply_block_txs(Txs, Miner, Trees, Height, Version, true) ->
    Trees0 = dsdc_trees:perform_pre_transformations(Trees, Height),
    case dsdc_trees:apply_txs_on_state_trees_strict(Txs, Trees0, Height, Version) of
        {ok, Txs1, Trees1} ->
            TotFee = calculate_total_fee(Txs1),
            Trees2 = dsdc_trees:grant_fee_to_miner(Miner, Trees1, TotFee),
            {ok, Txs1, Trees2, TotFee};
        Err = {error, _} ->
            Err
    end.

