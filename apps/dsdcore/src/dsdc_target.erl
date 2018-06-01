-module(dsdc_target).

%% API
-export([recalculate/2,
         determine_delta_header_height/1,
         verify/2]).

-include("blocks.hrl").

%% Return height of the header to be used as a start point for target calculations,
%% based on the following formula:
%% delta_height(Header) = Header.height - dsdc_governance:blocks_to_check_difficulty_count().
%% Returns {error | chain_too_short_to_recalculate_target} if initial height is a negative value
%% or it points to genesis block.
-spec determine_delta_header_height(
        dsdc_headers:header()) -> {ok, non_neg_integer()}
                         | {error, chain_too_short_to_recalculate_target}.
determine_delta_header_height(Header) ->
    Height = dsdc_headers:height(Header),
    BlocksCount = dsdc_governance:blocks_to_check_difficulty_count(),
    InitialHeight = Height - BlocksCount,
    GenesisHeight = dsdc_block_genesis:height(),
    case InitialHeight > GenesisHeight of
        true ->
            {ok, InitialHeight};
        false ->
            {error, chain_too_short_to_recalculate_target}
    end.

%% Target recalculation.
%%
%% Some concepts:
%%
%%    Difficulty = HIGHEST_TARGET / Target
%%    Rate       = Capacity / Difficulty  (blocks/ms)
%%    Capacity   = number of potential solutions per ms generated by miners
%%
%%    DesiredTimeBetweenBlocks = dsdc_governance:expected_block_mine_rate()
%%    DesiredRate              = 1 / DesiredTimeBetweenBlocks
%%
%% The basic idea of the algorithm is to estimate the current network capacity
%% based on the `N` (= 10) previous blocks and use that to set the new
%% target:
%%
%%    NewDifficulty = EstimatedCapacity / DesiredRate
%%    NewTarget     = HIGHEST_TARGET / NewDifficulty
%%                  = HIGHEST_TARGET * DesiredRate / EstimatedCapacity
%%
%% We can estimate the network capacity used to mine a given block `i` as
%%
%%    EstimatedCapacity[i] = Difficulty[i] / MiningTime[i]
%%    MiningTime[i]        = Time[i + 1] - Time[i]
%%
%% The estimated capacity across all `N` blocks is then the weighted (by time)
%% average of the estimated capacities for each block.
%%
%%    EstimatedCapacity = Sum(EstimatedCapacity[i] * MiningTime[i]) / TotalTime
%%                      = Sum(Difficulty[i]) / TotalTime
%%                      = Sum(HIGHEST_TARGET / Target[i]) / TotalTime
%%
%% Now, the problem is that we can't do any floating point arithmetic (to
%% ensure the calculation can be verified by other nodes), so we pick a
%% reasonably big integer K (= HIGHEST_TARGET * 2^32) and compute
%%
%%    EstimatedCapacity ≈ Sum(K * HIGHEST_TARGET div Target[i]) / TotalTime / K
%%
%% Then
%%
%%    NewTarget = HIGHEST_TARGET * DesiredRate / EstimatedCapacity
%%              ≈ HIGHEST_TARGET * DesiredRate * TotalTime * K / Sum(K * HIGHEST_TARGET div Target[i])
%%              ≈ DesiredRate * TotalTime * K / Sum(K div Target[i])
%%              ≈ TotalTime * K div (DesiredTimeBetweenBlocks * Sum(K div Target[i]))
%%
-spec recalculate(dsdc_headers:header(), nonempty_list(dsdc_headers:header())) -> non_neg_integer().
recalculate(Top, PrevHeaders0) ->
    %% Ensure the list of previous headers are in order - oldest first.
    PrevHeaders              = lists:keysort(#header.height, PrevHeaders0),
    K                        = dsdc_pow:scientific_to_integer(?HIGHEST_TARGET_SCI) * (1 bsl 32),
    SumKDivTargets           = lists:sum([ K div dsdc_pow:scientific_to_integer(dsdc_headers:target(Hd))
                                           || Hd <- PrevHeaders ]),
    DesiredTimeBetweenBlocks = dsdc_governance:expected_block_mine_rate(),
    Last                     = hd(PrevHeaders), %% Oldest first!
    TotalTime                = mining_time_between(Last, Top),
    NewTargetInt             = TotalTime * K div (DesiredTimeBetweenBlocks * SumKDivTargets),
    min(?HIGHEST_TARGET_SCI, dsdc_pow:integer_to_scientific(NewTargetInt)).

-spec verify(dsdc_headers:header(), nonempty_list(dsdc_headers:header())) ->
          ok | {error, {wrong_target, non_neg_integer(), non_neg_integer()}}.
verify(Top, PrevHeaders) ->
    HeaderTarget = dsdc_headers:target(Top),
    ExpectedTarget = recalculate(Top, PrevHeaders),
    case HeaderTarget == ExpectedTarget of
        true ->
            ok;
        false ->
            {error, {wrong_target, HeaderTarget, ExpectedTarget}}
    end.

%% Internals

-spec mining_time_between(dsdc_headers:header(), dsdc_headers:header()) -> integer().
mining_time_between(Header1, Header2) ->
    Time1 = dsdc_headers:time_in_msecs(Header1),
    Time2 = dsdc_headers:time_in_msecs(Header2),
    max(1, Time2 - Time1).

