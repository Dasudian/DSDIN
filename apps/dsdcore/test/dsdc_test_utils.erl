
-module(dsdc_test_utils).

-export([ mock_time/0
        , unmock_time/0
        , mock_difficulty_as_target/0
        , unmock_difficulty_as_target/0
        , mock_block_target_validation/0
        , unmock_block_target_validation/0
        , mock_fast_cuckoo_pow/0
        , mock_fast_and_deterministic_cuckoo_pow/0
        , mock_genesis/0
        , mock_genesis/1
        , unmock_genesis/0
        , wait_for_it/2
        , wait_for_it_or_timeout/3
        , exec_with_timeout/2
        , start_chain_db/0
        , stop_chain_db/0
        , extend_block_chain_with_state/3
        , dsdc_keys_setup/0
        , dsdc_keys_cleanup/1
        , dsdc_keys_bare_setup/0
        , dsdc_keys_bare_cleanup/1
        , gen_block_chain_with_state/1
        , gen_blocks_only_chain/1
        , gen_block_chain_with_state/2
        , blocks_only_chain/1
        , genesis_block/0
        , genesis_block_with_state/0
        , genesis_block_with_state/1
        , preset_accounts/0
        , genesis_accounts_balances/1
        , create_state_tree/0
        , create_state_tree_with_account/1
        , create_state_tree_with_accounts/1
        , create_temp_key_dir/0
        , remove_temp_key_dir/1
        , copy_genesis_dir/2
        , signed_spend_tx/1
        , fake_start_dsdhttp/0
        ]).

-include_lib("eunit/include/eunit.hrl").
-include("blocks.hrl").

-ifdef(DEBUG).
-define(ifDebugFmt(Str, Args), ?debugFmt(Str, Args)).
-else.
-define(ifDebugFmt(Str, Args), ok).
-endif.

-define(PRESET_ACCOUNTS, [{<<"my_public_key">>, 100}]).

preset_accounts() ->
  ?PRESET_ACCOUNTS.

genesis_accounts_balances(PresetAccounts) ->
    [{dsdc_block_genesis:miner(), dsdc_governance:block_mine_reward()}
     | PresetAccounts].

mock_time() ->
    meck:new(dsdu_time, [passthrough]),
    TS = spawn(fun() -> receive F -> F(F, 1000) end end),
    TS ! fun(F, T) ->
                 receive {time, R, From}  -> From ! {R,T}
                 end,
                 F(F, T + 1000)
         end,
    GetTime = fun () -> TS ! {time, R = make_ref(), self()},
                        receive {R, T} -> T end
              end,

    meck:expect(dsdu_time, now_in_msecs,
                fun() ->
                        GetTime()
                end),
    ok.

unmock_time() ->
    meck:unload(dsdu_time).

mock_fast_cuckoo_pow() ->
    mock_fast_cuckoo_pow({"mean16s-generic", "-t 5", 16}).

mock_fast_and_deterministic_cuckoo_pow() ->
    mock_fast_cuckoo_pow({"mean16s-generic", "", 16}).

mock_fast_cuckoo_pow({_MinerBin, _MinerExtraArgs, _NodeBits} = Cfg) ->
    meck:expect(dsdu_env, get_env, 3,
                fun
                    (dsdcore, dsdc_pow_cuckoo, _) ->
                       Cfg;
                    (App, Key, Def) ->
                       meck:passthrough([App, Key, Def])
               end).

mock_genesis() ->
    mock_genesis(preset_accounts()).

mock_genesis(PresetAccounts) ->
    meck:new(dsdc_genesis_block_settings, []),
    meck:expect(dsdc_genesis_block_settings, preset_accounts, 0, PresetAccounts),
    ok.

unmock_genesis() ->
    meck:unload(dsdc_genesis_block_settings),
    ok.

wait_for_it(Fun, Value) ->
    wait_for_it(Fun, Value, 0).

wait_for_it(Fun, Value, Sleep) ->
    case Fun() of
        Value ->
            Value;
        _Other ->
            ?ifDebugFmt("Waiting for ~p got ~p~n",[Value,_Other]),
            timer:sleep(Sleep),
            wait_for_it(Fun, Value, Sleep + 10)
    end.

wait_for_it_or_timeout(Fun, Value, Timeout) ->
    exec_with_timeout(fun() -> wait_for_it(Fun, Value) end, Timeout).

exec_with_timeout(Fun, Timeout)  when is_function(Fun, 0) ->
    Pid = self(),
    spawn(
        fun() ->
            Res = Fun(),
            Pid ! {exec_result, Res}
        end),
    receive
        {exec_result, Res} ->
            {ok, Res}
    after
        Timeout ->
            {error, timeout}
    end.


%%%=============================================================================
%%% Chain related util functions
%%%=============================================================================

%% @doc Meck difficulty as target. And startup keys server

%% Make mecked difficulty calculation realistic:
%% - when target increases, difficulty decreases
%% - when target decreases, difficulty increases
-define(FACTOR, 1000000000).
mock_difficulty_as_target() ->
    meck:new(dsdc_headers, [passthrough]),
    meck:new(dsdc_blocks, [passthrough]),
    meck:expect(dsdc_headers, difficulty,
                fun(#header{target = T}) when is_integer(T) ->
                        float(trunc(?FACTOR / T))
                end),
    meck:expect(dsdc_blocks, difficulty,
                fun(#block{target = T}) when is_integer(T) ->
                        float(trunc(?FACTOR / T))
                end).

unmock_difficulty_as_target() ->
    meck:unload(dsdc_headers),
    meck:unload(dsdc_blocks).


mock_block_target_validation() ->
    meck:new(dsdc_governance, [passthrough]),
    meck:new(dsdc_target, [passthrough]),
    meck:expect(dsdc_governance, blocks_to_check_difficulty_count, 0, 1),
    meck:expect(dsdc_target, verify, 2, ok).

unmock_block_target_validation() ->
    meck:unload(dsdc_governance),
    meck:unload(dsdc_target).


start_chain_db() ->
    ok = mnesia:start(),
    ok = dsdc_db:initialize_db(ram),
    Tabs = [Tab || {Tab, _} <- dsdc_db:tables(ram)],
    ok = mnesia:wait_for_tables(Tabs, 5000).

stop_chain_db() ->
    application:stop(mnesia).

genesis_block() ->
    {B, _} = genesis_block_with_state(),
    B.

genesis_block_with_state() ->
    genesis_block_with_state(?PRESET_ACCOUNTS).

genesis_block_with_state(PresetAccounts) ->
    dsdc_block_genesis:genesis_block_with_state(#{preset_accounts => PresetAccounts}).

%% Generic blockchain without transactions
gen_block_chain_with_state(Length) ->
    gen_block_chain_with_state(Length, ?PRESET_ACCOUNTS).

gen_blocks_only_chain(Length) ->
    blocks_only_chain(gen_block_chain_with_state(Length)).

gen_block_chain_with_state(Length, PresetAccounts) when Length > 0 ->
    {ok, MinerAccount} = dsdc_keys:wait_for_pubkey(),
    gen_block_chain_with_state(Length, MinerAccount, PresetAccounts, []).


gen_block_chain_with_state(0,_MinerAccount, _PresetAccounts, Acc) -> lists:reverse(Acc);
gen_block_chain_with_state(N, MinerAccount, PresetAccounts, []) ->
    {B, S} = dsdc_block_genesis:genesis_block_with_state(#{preset_accounts => PresetAccounts}),
    gen_block_chain_with_state(N - 1, MinerAccount, PresetAccounts, [{B, S}]);
gen_block_chain_with_state(N, MinerAccount, PresetAccounts, [{PreviousBlock, Trees} | _] = Acc) ->
    {B, S} = dsdc_block_candidate:create_with_state(PreviousBlock, MinerAccount, [], Trees),
    gen_block_chain_with_state(N - 1, MinerAccount, PresetAccounts, [{B, S} | Acc]).

extend_block_chain_with_state(PrevBlock, PrevBlockState, Data) ->
    {ok, MinerAccount} = dsdc_keys:wait_for_pubkey(),
    Targets    = maps:get(targets, Data),
    TxsFun     = maps:get(txs_by_height_fun, Data),
    Nonce      = maps:get(nonce, Data, 12345),
    Timestamps = maps:get(timestamps, Data, lists:duplicate(length(Targets), undefined)),
    extend_block_chain_with_state(PrevBlock, PrevBlockState, Targets, Timestamps, TxsFun, Nonce, MinerAccount, []).


extend_block_chain_with_state(_, _, [], _, _, _, _, Chain) ->
    lists:reverse(Chain);
extend_block_chain_with_state(PrevBlock, PrevBlockState, [Tgt | Tgts], [Ts | Tss], TxsFun, Nonce, MinerAcc, Chain) ->
    {Block, BlockState} = next_block_with_state(PrevBlock, PrevBlockState, Tgt, Ts, TxsFun, Nonce, MinerAcc),
    extend_block_chain_with_state(Block, BlockState, Tgts, Tss, TxsFun, Nonce, MinerAcc, [{Block, BlockState} | Chain]).


blocks_only_chain(Chain) ->
    lists:map(fun({B, _S}) -> B end, Chain).


next_block_with_state(PrevBlock, Trees, Target, Time0, TxsFun, Nonce, MinerAcc) ->
    Height = dsdc_blocks:height(PrevBlock) + 1,
    Txs = TxsFun(Height),
    {B, S} = dsdc_block_candidate:create_with_state(PrevBlock, MinerAcc, Txs, Trees),
    {B#block{ target = Target, nonce  = Nonce,
              time   = case Time0 of undefined -> B#block.time; _ -> Time0 end },
     S}.

signed_spend_tx(ArgsMap) ->
    {ok, SenderAccount} = dsdc_keys:wait_for_pubkey(),
    ArgsMap1 = maps:put(sender, SenderAccount, ArgsMap),
    {ok, SpendTx} = dsdc_spend_tx:new(ArgsMap1),
    {ok, SSTx} = dsdc_keys:sign(SpendTx),
    SSTx.

%% function to setup the .genesis file for test SUITE-s
%% SourceGenesisDir is the test release directory from which to take the .genesis
%% DestRelDir is the release being set up
copy_genesis_dir(SourceRelDir, DestRelDir) ->
    GenesisDir = dsdc_genesis_block_settings:dir(), % ex data/dsdcore/.genesis
    GenesisName = filename:basename(GenesisDir),
    DataAecoreRoot = filename:dirname(GenesisDir), % ex. data/dsdcore
    DataRoot = filename:dirname(DataAecoreRoot), % ex. data
    AecoreName = filename:basename(DataAecoreRoot),
    DataRootName = filename:basename(DataRoot),
    DestGenesisDir = filename:join([DestRelDir, DataRootName, AecoreName, GenesisName]),
    ok = filelib:ensure_dir(DestGenesisDir ++ "/"),
    SourceGenesisDir = filename:join([SourceRelDir, DataRootName, AecoreName, GenesisName]),
    {ok, AllFiles} = file:list_dir_all(SourceGenesisDir),
    lists:foreach(
        fun(FileName) ->
            {ok, _} = file:copy(filename:join(SourceGenesisDir, FileName),
                                filename:join(DestGenesisDir, FileName))
        end,
        AllFiles).



%%%=============================================================================
%%% Key server setup/teardown
%%%=============================================================================

dsdc_keys_setup() ->
    ok = application:ensure_started(crypto),
    dsdc_keys_bare_setup().

dsdc_keys_cleanup(TmpKeysDir) ->
    dsdc_keys_bare_cleanup(TmpKeysDir),
    ok = application:stop(crypto).

dsdc_keys_bare_setup() ->
    TmpKeysDir = create_temp_key_dir(),
    {ok, _} = dsdc_keys:start_link([<<"mypassword">>, TmpKeysDir]),
    wait_for_it(fun() -> whereis(dsdc_keys) =/= undefined end, true),
    TmpKeysDir.

dsdc_keys_bare_cleanup(TmpKeysDir) ->
    ok = dsdc_keys:stop(),
    remove_temp_key_dir(TmpKeysDir).

remove_temp_key_dir(TmpKeysDir) ->
    {ok, KeyFiles} = file:list_dir(TmpKeysDir),
    %% Expect four filenames - private and public keys x2.
    [_KF1, _KF2, _KF3, _KF4] = KeyFiles,
    lists:foreach(
      fun(F) ->
              AbsF = filename:absname_join(TmpKeysDir, F),
              {ok, _} = {file:delete(AbsF), {F, AbsF}}
      end,
      KeyFiles),
    ok = file:del_dir(TmpKeysDir).

create_temp_key_dir() ->
    mktempd(os:type()).

mktempd({unix, _}) ->
    lib:nonl(?cmd("mktemp -d")).

fake_start_dsdhttp() ->
    gproc:reg({n,l,{epoch, app, dsdhttp}}).


%%%=============================================================================
%%% State trees setup
%%%=============================================================================

create_state_tree() ->
    dsdc_trees:new().

create_state_tree_with_account(Account) ->
    create_state_tree_with_accounts([Account]).

create_state_tree_with_accounts(Accounts) ->
    StateTrees0 = create_state_tree(),
    AccountsTree0 = dsdc_trees:accounts(StateTrees0),
    AccountsTree1 = lists:foldl(fun dsdc_accounts_trees:enter/2,
                                AccountsTree0, Accounts),
    dsdc_trees:set_accounts(StateTrees0, AccountsTree1).

