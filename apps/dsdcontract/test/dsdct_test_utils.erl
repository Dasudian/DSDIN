
-module(dsdct_test_utils).

-export([ new_state/0
        , calls/1
        , set_calls/2
        , contracts/1
        , set_contracts/2
        , priv_key/2
        , call_tx/3
        , call_tx/4
        , create_tx/2
        , create_tx/3
        , set_account_balance/3
        , set_trees/2
        , setup_new_account/1
        , setup_new_account/2
        , setup_miner_account/2
        , get_account/2
        , next_nonce/2
        , trees/1
        , compile_contract/1
        , assert_state_equal/2
        ]).

-include_lib("apps/dsdcontract/include/contract_txs.hrl").

%%%===================================================================
%%% Test state
%%%===================================================================

new_state() ->
    #{}.

trees(#{} = S) ->
    maps:get(trees, S, dsdc_trees:new()).

set_trees(Trees, S) ->
    S#{trees => Trees}.

insert_key_pair(Pub, Priv, S) ->
    Old = key_pairs(S),
    S#{key_pairs => Old#{Pub => Priv}}.

key_pairs(S) -> maps:get(key_pairs, S, #{}).

next_nonce(PubKey, S) ->
    Account = dsdc_accounts_trees:get(PubKey, dsdc_trees:accounts(trees(S))),
    dsdc_accounts:nonce(Account) + 1.

priv_key(PubKey, State) ->
    maps:get(PubKey, key_pairs(State)).

%% Errs if actual test state is different from expected one.
assert_state_equal(Exp, Act) ->
    case {maps:take(trees, Exp), maps:take(trees, Act)} of
        {error, error} ->
            {Exp, _} = {Act, {expected_state, Exp}};
        {{ExpTs, Exp2}, {ActTs, Act2}} ->
            ExpTsHash = dsdc_trees:hash(ExpTs),
            ActTsHash = dsdc_trees:hash(ActTs),
            {ExpTsHash, _} = {ActTsHash,
                              {{expected_trees_root_hash, ExpTsHash},
                               {trees, {{actual, ActTs},
                                        {expected, ExpTs}}}}},
            {Exp2, _} = {Act2, {expected_state_except_trees, Exp2}},
            ok
    end.

%%%===================================================================
%%% Info API
%%%===================================================================

calls(State) ->
    dsdc_trees:calls(trees(State)).

set_calls(Calls, State) ->
    Trees = trees(State),
    set_trees(dsdc_trees:set_calls(Trees, Calls), State).

contracts(State) ->
    dsdc_trees:contracts(trees(State)).

set_contracts(Contracts, State) ->
    Trees = trees(State),
    set_trees(dsdc_trees:set_contracts(Trees, Contracts), State).

%%%===================================================================
%%% Register tx
%%%===================================================================

create_tx(PubKey, State) ->
    create_tx(PubKey, #{}, State).

create_tx(PubKey, Spec0, State) ->
    Spec = maps:merge(create_tx_default_spec(PubKey, State), Spec0),
    dsdtx:new(dsdct_create_tx,
             #contract_create_tx{ owner      = PubKey
                                , nonce      = maps:get(nonce, Spec)
                                , fee        = maps:get(fee, Spec)
                                , ttl        = maps:get(ttl, Spec)
                                , code       = maps:get(code, Spec)
                                , vm_version = maps:get(vm_version, Spec)
                                , deposit    = maps:get(deposit, Spec)
                                , amount     = maps:get(amount, Spec)
                                , gas        = maps:get(gas, Spec)
                                , gas_price  = maps:get(gas_price, Spec)
                                , call_data  = maps:get(call_data, Spec)
                                }).

create_tx_default_spec(PubKey, State) ->
    #{ fee        => 5
     , ttl        => 100
     , nonce      => try next_nonce(PubKey, State) catch _:_ -> 0 end
     , code       => <<"NOT PROPER BYTE CODE">>
     , vm_version => 1
     , deposit    => 10
     , amount     => 200
     , gas        => 10
     , gas_price  => 1
     , call_data  => <<"NOT ENCODED ACCORDING TO ABI">>
     }.

%%%===================================================================
%%% Call tx
%%%===================================================================

call_tx(PubKey, ContractKey, State) ->
    call_tx(PubKey, ContractKey, #{}, State).

call_tx(PubKey, ContractKey, Spec0, State) ->
    Spec = maps:merge(call_tx_default_spec(PubKey, State), Spec0),
    dsdtx:new(dsdct_call_tx,
             #contract_call_tx{ caller     = PubKey
                              , nonce      = maps:get(nonce, Spec)
                              , contract   = ContractKey
                              , vm_version = maps:get(vm_version, Spec)
                              , fee        = maps:get(fee, Spec)
                              , ttl        = maps:get(ttl, Spec)
                              , amount     = maps:get(amount, Spec)
                              , gas        = maps:get(gas, Spec)
                              , gas_price  = maps:get(gas_price, Spec)
                              , call_data  = maps:get(call_data, Spec)
                              }).

call_tx_default_spec(PubKey, State) ->
    #{ fee         => 5
     , ttl         => 100
     , nonce       => try next_nonce(PubKey, State) catch _:_ -> 0 end
     , vm_version  => 1
     , amount      => 100
     , gas         => 10000
     , gas_price   => 1
     , call_data   => <<"CALL DATA">>
     }.

%%%===================================================================
%%% Accounts
%%%===================================================================

setup_new_account(State) ->
    setup_new_account(1000000, State).

setup_new_account(Balance, State) ->
    {PubKey, PrivKey} = new_key_pair(),
    State1            = insert_key_pair(PubKey, PrivKey, State),
    State2            = set_account(dsdc_accounts:new(PubKey, Balance), State1),
    {PubKey, State2}.

setup_miner_account(PubKey, State) ->
    A = dsdc_accounts:new(PubKey, dsdc_governance:block_mine_reward()),
    set_account(A, State).

set_account_balance(PubKey, NewBalance, State) ->
    A        = get_account(PubKey, State),
    Balance  = dsdc_accounts:balance(A),
    Nonce    = dsdc_accounts:nonce(A),
    {ok, A1} = dsdc_accounts:spend(A, Balance, Nonce),
    {ok, A2} = dsdc_accounts:earn(A1, NewBalance),
    set_account(A2, State).

get_account(PubKey, State) ->
    dsdc_accounts_trees:get(PubKey, dsdc_trees:accounts(trees(State))).

set_account(Account, State) ->
    Trees   = trees(State),
    AccTree = dsdc_accounts_trees:enter(Account, dsdc_trees:accounts(Trees)),
    set_trees(dsdc_trees:set_accounts(Trees, AccTree), State).

compile_contract(File) ->
    CodeDir = code:lib_dir(dsdsophia, test),
    FileName = filename:join(CodeDir, File),
    {ok, ContractBin} = file:read_file(FileName),
    Contract = binary_to_list(ContractBin),
    dsdso_compiler:from_string(Contract, []). % [pp_icode, pp_assembler, pp_bytecode]).

new_key_pair() ->
    #{ public := PubKey, secret := PrivKey } = enacl:sign_keypair(),
    {PubKey, PrivKey}.

