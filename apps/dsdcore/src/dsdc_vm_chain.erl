
-module(dsdc_vm_chain).

-behaviour(dsdvm_chain_api).

-export([new_state/3, get_trees/1]).

%% dsdvm_chain_api callbacks
-export([get_balance/2,
	 get_store/1,
	 set_store/2,
         spend/3,
         call_contract/6]).

-record(state, { trees   :: dsdc_trees:trees()
               , height  :: dsdc_blocks:height()
               , account :: dsdc_keys:pubkey()            %% the contract account
               }).

-type chain_state() :: #state{}.

-define(PUB_SIZE, 32).

%% -- API --------------------------------------------------------------------

%% @doc Create a chain state.
-spec new_state(dsdc_trees:trees(), dsdc_blocks:height(), dsdc_keys:pubkey()) -> chain_state().
new_state(Trees, Height, ContractAccount) ->
    #state{ trees   = Trees,
            height  = Height,
            account = ContractAccount
          }.

%% @doc Get the state trees from a state.
-spec get_trees(chain_state()) -> dsdc_trees:trees().
get_trees(#state{ trees = Trees}) ->
    Trees.

%% @doc Get the balance of the contract account.
-spec get_balance(dsdc_keys:pubkey(), chain_state()) -> non_neg_integer().
get_balance(PubKey, #state{ trees = Trees }) ->
    do_get_balance(PubKey, Trees).

%% @doc Get the contract state store of the contract account.
-spec get_store(chain_state()) -> dsdvm_chain_api:store().
get_store(#state{ account = PubKey, trees = Trees }) ->
    Store = do_get_store(PubKey, Trees),
    Store.

%% @doc Set the contract state store of the contract account.
-spec set_store(dsdvm_chain_api:store(), chain_state()) -> chain_state().
set_store(Store,  #state{ account = PubKey, trees = Trees } = State) ->
    CTree1 = do_set_store(Store, PubKey, Trees),
    Trees1 = dsdc_trees:set_contracts(Trees, CTree1),
    State#state{ trees = Trees1 }.


%% @doc Spend money from the contract account.
-spec spend(dsdc_keys:pubkey(), non_neg_integer(), chain_state()) ->
          {ok, chain_state()} | {error, term()}.
spend(Recipient, Amount, State = #state{ trees   = Trees,
                                         height  = Height,
                                         account = ContractKey }) ->
    case do_spend(Recipient, ContractKey, Amount, Trees, Height) of
        {ok, Trees1}     -> {ok, State#state{ trees = Trees1 }};
        Err = {error, _} -> Err
    end.

%% @doc Call another contract.
-spec call_contract(dsdc_keys:pubkey(), non_neg_integer(), non_neg_integer(), binary(),
                    [non_neg_integer()], chain_state()) ->
        {ok, dsdvm_chain_api:call_result(), chain_state()} | {error, term()}.
call_contract(Target, Gas, Value, CallData, CallStack,
              State = #state{ trees   = Trees,
                              height  = Height,
                              account = ContractKey }) ->
    ConsensusVersion = dsdc_hard_forks:protocol_effective_at_height(Height),
    CT = dsdc_trees:contracts(Trees),
    case dsdct_state_tree:lookup_contract(Target, CT) of
        {value, Contract} ->
            AT = dsdc_trees:accounts(Trees),
            {value, ContractAccount} = dsdc_accounts_trees:lookup(ContractKey, AT),
            Nonce = dsdc_accounts:nonce(ContractAccount) + 1,
            VmVersion = dsdct_contracts:vm_version(Contract),
            {ok, CallTx} =
                dsdct_call_tx:new(#{ caller     => ContractKey,
                                    nonce      => Nonce,
                                    contract   => Target,
                                    vm_version => VmVersion,
                                    fee        => 0,
                                    ttl        => Height,
                                    amount     => Value,
                                    gas        => Gas,
                                    gas_price  => 0,
                                    call_data  => CallData,
                                    call_stack => CallStack }),
            do_call_contract(CallTx, ContractKey, Target, Nonce, Trees, State, Height,
                             ConsensusVersion);
        none -> {error, {no_such_contract, Target}}
    end.

do_call_contract(CallTx, ContractKey, Target, Nonce, Trees,
                 State, Height, ConsensusVersion) ->
    case dsdtx:check_from_contract(CallTx, Trees, Height, ConsensusVersion) of
        Err = {error, _} -> Err;
        {ok, Trees1} ->
            {ok, Trees2} =
                dsdtx:process_from_contract(CallTx, Trees1,
                                           Height, ConsensusVersion),
            CallId  = dsdct_call:id(ContractKey, Nonce, Target),
            Call    = dsdct_call_state_tree:get_call(Target, CallId,
                                                    dsdc_trees:calls(Trees2)),
            GasUsed = dsdct_call:gas_used(Call),
            Result  =
                case dsdct_call:return_type(Call) of
                    %% TODO: currently we don't set any
                    %%       sensible return value on exceptions
                    error ->
                        dsdvm_chain_api:call_exception(out_of_gas, GasUsed);
                    ok ->
                        Bin = dsdct_call:return_value(Call),
                        dsdvm_chain_api:call_result(Bin, GasUsed)
                end,
            {ok, Result, State#state{ trees = Trees2}}
    end.


%% -- Internal functions -----------------------------------------------------

do_get_balance(PubKey, Trees) ->
    AccountsTree  = dsdc_trees:accounts(Trees),
    case dsdc_accounts_trees:lookup(PubKey, AccountsTree) of
        none             -> 0;
        {value, Account} -> dsdc_accounts:balance(Account)
    end.

do_get_store(PubKey, Trees) ->
    ContractsTree = dsdc_trees:contracts(Trees),
    case dsdct_state_tree:lookup_contract(PubKey, ContractsTree) of
        {value, Contract} -> dsdct_contracts:state(Contract);
        none              -> #{}
    end.

do_set_store(Store, PubKey, Trees) ->
    ContractsTree = dsdc_trees:contracts(Trees),
    NewContract =
	case dsdct_state_tree:lookup_contract(PubKey, ContractsTree) of
	    {value, Contract} -> dsdct_contracts:set_state(Store, Contract)
	end,
    dsdct_state_tree:enter_contract(NewContract, ContractsTree).

do_spend(Recipient, ContractKey, Amount, Trees, Height) ->
    AccountTree = dsdc_trees:accounts(Trees),
    {value, Account} = dsdc_accounts_trees:lookup(ContractKey, AccountTree),
    Nonce = dsdc_accounts:nonce(Account) + 1,
    ConsensusVersion = dsdc_hard_forks:protocol_effective_at_height(Height),
    {ok, SpendTx} = dsdc_spend_tx:new(#{ sender => ContractKey
                                      , recipient => Recipient
                                      , amount => Amount
                                      , fee => 0
                                      , ttl => Height
                                      , nonce => Nonce
                                      , payload => <<>>}),
    case dsdtx:check_from_contract(SpendTx, Trees, Height, ConsensusVersion) of
        {ok, Trees1} ->
            dsdtx:process_from_contract(SpendTx, Trees1, Height, ConsensusVersion);
        Error -> Error
    end.


