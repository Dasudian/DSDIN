
-module(dsdct_create_tx).

-include("dsdcontract.hrl").
-include("contract_txs.hrl").

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
-export([owner/1,
         code/1,
         vm_version/1,
         deposit/1,
         amount/1,
         gas/1,
         gas_price/1,
         call_data/1]).

-define(CONTRACT_CREATE_TX_VSN, 1).
-define(CONTRACT_CREATE_TX_TYPE, contract_create_tx).
-define(CONTRACT_CREATE_TX_FEE, 4).

%% Should this be in a header file somewhere?
-define(PUB_SIZE, 32).

-type amount() :: dsdct_contracts:amount().

-opaque tx() :: #contract_create_tx{}.

-export_type([tx/0]).

%%%===================================================================
%%% Getters

-spec owner(tx()) -> dsdc_keys:pubkey().
owner(#contract_create_tx{owner = OwnerPubKey}) ->
    OwnerPubKey.

-spec code(tx()) -> binary().
code(#contract_create_tx{code = X}) ->
    X.

-spec vm_version(tx()) -> dsdct_contracts:vm_version().
vm_version(#contract_create_tx{vm_version = X}) ->
    X.

-spec deposit(tx()) -> amount().
deposit(#contract_create_tx{deposit = X}) ->
    X.

-spec amount(tx()) -> amount().
amount(#contract_create_tx{amount = X}) ->
    X.

-spec gas(tx()) -> amount().
gas(#contract_create_tx{gas = X}) ->
    X.

-spec gas_price(tx()) -> amount().
gas_price(#contract_create_tx{gas_price = X}) ->
    X.

-spec call_data(tx()) -> binary().
call_data(#contract_create_tx{call_data = X}) ->
    X.

%%%===================================================================
%%% Behavior API

-spec fee(tx()) -> integer().
fee(#contract_create_tx{fee = Fee}) ->
    Fee.

-spec ttl(tx()) -> dsdc_blocks:height().
ttl(#contract_create_tx{ttl = TTL}) ->
    TTL.

-spec new(map()) -> {ok, dsdtx:tx()}.
new(#{owner      := OwnerPubKey,
      nonce      := Nonce,
      code       := Code,
      vm_version := VmVersion,
      deposit    := Deposit,
      amount     := Amount,
      gas        := Gas,
      gas_price  := GasPrice,
      call_data  := CallData,
      fee        := Fee,
      ttl        := TTL}) ->
    Tx = #contract_create_tx{owner      = OwnerPubKey,
                             nonce      = Nonce,
                             code       = Code,
                             vm_version = VmVersion,
                             deposit    = Deposit,
                             amount     = Amount,
                             gas        = Gas,
                             gas_price  = GasPrice,
                             call_data  = CallData,
                             fee        = Fee,
                             ttl        = TTL},
    {ok, dsdtx:new(?MODULE, Tx)}.

-spec type() -> atom().
type() ->
    ?CONTRACT_CREATE_TX_TYPE.

-spec nonce(tx()) -> non_neg_integer().
nonce(#contract_create_tx{nonce = Nonce}) ->
    Nonce.

-spec origin(tx()) -> dsdc_keys:pubkey().
origin(#contract_create_tx{owner = OwnerPubKey}) ->
    OwnerPubKey.

%% Owner should exist, and have enough funds for the fee, the amount
%% the deposit and the gas
-spec check(tx(), dsdtx:tx_context(), dsdc_trees:trees(), dsdc_blocks:height(), non_neg_integer()) ->
                   {ok, dsdc_trees:trees()} | {error, term()}.
check(#contract_create_tx{owner = OwnerPubKey,
                          nonce = Nonce,
                          amount     = Amount,
                          gas        = Gas,
                          gas_price  = GasPrice,
                          deposit    = Deposit,
                          fee = Fee}, _Context, Trees, _Height, _ConsensusVersion) ->
    TotalAmount = Fee + Amount + Deposit + Gas * GasPrice,
    Checks =
        [fun() -> dsdtx_utils:check_account(OwnerPubKey, Trees, Nonce, TotalAmount) end
         %% TODO: Check minum gas price.
        ],
    case dsdu_validation:run(Checks) of
        ok              -> {ok, Trees};
        {error, Reason} -> {error, Reason}
    end.

-spec accounts(tx()) -> [dsdc_keys:pubkey()].
accounts(#contract_create_tx{owner = OwnerPubKey}) ->
    [OwnerPubKey].

-spec signers(tx(), dsdc_trees:trees()) -> {ok, [dsdc_keys:pubkey()]}.
signers(#contract_create_tx{owner = OwnerPubKey}, _) ->
    {ok, [OwnerPubKey]}.

-spec process(tx(), dsdtx:tx_context(), dsdc_trees:trees(),
              dsdc_blocks:height(), non_neg_integer()) -> {ok, dsdc_trees:trees()}.
process(#contract_create_tx{owner      = OwnerPubKey,
                            nonce      = Nonce,
			    vm_version = _VmVersion,
                            amount     = Amount,
                            gas        = _Gas,
                            gas_price  = GasPrice,
                            deposit    = _Deposit,
                            fee        = Fee} = CreateTx,
        Context, Trees0, Height, ConsensusVersion) ->

    {ContractPubKey, Contract, Trees1} =
        create_contract(OwnerPubKey, Nonce, CreateTx, Trees0),

    %% Charge the fee to the contract owner (caller)
    %% and transfer the funds (amount) to the contract account.
    Trees2 =
        spend(OwnerPubKey, ContractPubKey, Amount, Fee,
              Nonce, Context, Height, Trees1, ConsensusVersion),

    %% Create the init call.
    Call0 = dsdct_call:new(OwnerPubKey, Nonce, ContractPubKey, Height),
    %% Execute init calQl to get the contract state and return value
    {CallRes, Trees3} =
        run_contract(CreateTx, Call0, Height, Trees2, Contract, ContractPubKey),

    case dsdct_call:return_type(CallRes) of
        ok ->
            initialize_contract(CreateTx, ContractPubKey, Contract,
                                CallRes, Context, Trees3, Height,
                                ConsensusVersion);
        E ->
            lager:debug("Init call error ~w ~w~n",[E, CallRes]),
            %% Don't create the contract if 'init' fails!
            %% Go back to Trees0 (Without contract or any account changes)
            %% Spend gas + fee
            %% (The VM will decide how much gas is used: 0, some, all.)
            GasCost = dsdct_call:gas_used(CallRes) * GasPrice,
            Trees5 =
                spend(OwnerPubKey, ContractPubKey, 0, Fee+GasCost, Nonce,
                      Context, Height, Trees1, ConsensusVersion),
            {ok, Trees5}
    end.


create_contract(OwnerPubKey, Nonce, CreateTx, Trees0) ->
    %% Create the contract and insert it into the contract state tree
    %%   The public key for the contract is generated from the owners pubkey
    %%   and the nonce, so that no one has the private key.
    ContractPubKey  = dsdct_contracts:compute_contract_pubkey(OwnerPubKey, Nonce),
    Contract        = dsdct_contracts:new(ContractPubKey, CreateTx),
    ContractsTree0  = dsdc_trees:contracts(Trees0),
    ContractsTree1  = dsdct_state_tree:insert_contract(Contract, ContractsTree0),
    Trees1          = dsdc_trees:set_contracts(Trees0, ContractsTree1),
    {ContractPubKey, Contract, Trees1}.


spend(SenderPubKey, ReceiverPubKey, Value, Fee, Nonce,
      Context, Height, Trees, ConsensusVersion) ->
    {ok, SpendTx} = dsdc_spend_tx:new(
                      #{ sender => SenderPubKey
                       , recipient => ReceiverPubKey
                       , amount => Value
                       , fee => Fee
                       , ttl => Height
                       , nonce => Nonce
                       , payload => <<>>}),
    Trees1 = dsdc_trees:ensure_account(ReceiverPubKey, Trees),
    case Context of
        dsdtx_contract ->
            {ok, Trees2} =
                dsdtx:process_from_contract(SpendTx, Trees1, Height, ConsensusVersion),
            Trees2;
        dsdtx_transaction ->
            {ok, Trees2} =
                dsdtx:process(SpendTx, Trees1, Height, ConsensusVersion),
            Trees2
    end.


run_contract(#contract_create_tx{ owner      = Caller
				, nonce      =_Nonce
				, code       = Code
				, vm_version = VmVersion
				, amount     =_Amount
				, gas        = Gas
				, gas_price  = GasPrice
				, call_data  = CallData
				} =_Tx,
	     Call, Height, Trees,_Contract, ContractPubKey)->
    CallStack = [], %% TODO: should we have a call stack for create_tx also
                    %% when creating a contract in a contract.

    CallDef = #{ caller     => Caller
	       , contract   => ContractPubKey
	       , gas        => Gas
	       , gas_price  => GasPrice
	       , call_data  => CallData
	       , amount     => 0 %% Initial call takes no amount
	       , call_stack => CallStack
	       , code       => Code
	       , call       => Call
	       , height     => Height
	       , trees      => Trees
	       },

    dsdct_dispatch:run(VmVersion, CallDef).

initialize_contract(#contract_create_tx{owner      = OwnerPubKey,
                                        nonce      = Nonce,
                                        vm_version = VmVersion,
                                        amount     = _Amount,
                                        gas        = _Gas,
                                        gas_price  = GasPrice,
                                        deposit    = Deposit,
                                        fee        = _Fee},
                    ContractPubKey, Contract,
                    CallRes,  Context, Trees, Height, ConsensusVersion) ->
    %% Insert the call into the state tree for one block.
    %% This is mainly to make the return value accessible.
    %% Each block starts with an empty calls tree.
    CallsTree0 = dsdc_trees:calls(Trees),
    CallsTree1 = dsdct_call_state_tree:insert_call(CallRes, CallsTree0),
    Trees1     = dsdc_trees:set_calls(Trees, CallsTree1),

    %% Spend Gas and burn
    %% Deposit (the deposit is stored in the contract.)
    GasCost = dsdct_call:gas_used(CallRes) * GasPrice,
    Trees2 =
        spend(OwnerPubKey, ContractPubKey, 0, Deposit+GasCost, Nonce,
              Context, Height, Trees1,
              ConsensusVersion),

    %% TODO: Move ABI specific code to abi module(s).
    Contract1 =
        case VmVersion of
            ?AEVM_01_Sophia_01 ->
                %% Save the initial state (returned by `init`) in the store.
                InitState  = dsdct_call:return_value(CallRes),
                %% TODO: move to/from_sophia_state to make nicer dependencies?
                dsdct_contracts:set_state(
                  dsdvm_eeevm_store:from_sophia_state(InitState), Contract);
            ?AEVM_01_Solidity_01 ->
                %% Solidity inital call returns the code to store in the contract.
                NewCode = dsdct_call:return_value(CallRes),
                dsdct_contracts:set_code(NewCode, Contract)
        end,
    ContractsTree0 = dsdc_trees:contracts(Trees2),
    ContractsTree1 = dsdct_state_tree:enter_contract(Contract1, ContractsTree0),
    {ok, dsdc_trees:set_contracts(Trees2, ContractsTree1)}.




serialize(#contract_create_tx{owner      = OwnerPubKey,
                              nonce      = Nonce,
                              code       = Code,
                              vm_version = VmVersion,
                              fee        = Fee,
                              ttl        = TTL,
                              deposit    = Deposit,
                              amount     = Amount,
                              gas        = Gas,
                              gas_price  = GasPrice,
                              call_data  = CallData}) ->
    {version(),
     [ {owner, OwnerPubKey}
     , {nonce, Nonce}
     , {code, Code}
     , {vm_version, VmVersion}
     , {fee, Fee}
     , {ttl, TTL}
     , {deposit, Deposit}
     , {amount, Amount}
     , {gas, Gas}
     , {gas_price, GasPrice}
     , {call_data, CallData}
     ]}.

deserialize(?CONTRACT_CREATE_TX_VSN,
            [ {owner, OwnerPubKey}
            , {nonce, Nonce}
            , {code, Code}
            , {vm_version, VmVersion}
            , {fee, Fee}
            , {ttl, TTL}
            , {deposit, Deposit}
            , {amount, Amount}
            , {gas, Gas}
            , {gas_price, GasPrice}
            , {call_data, CallData}]) ->
    #contract_create_tx{owner      = OwnerPubKey,
                        nonce      = Nonce,
                        code       = Code,
                        vm_version = VmVersion,
                        fee        = Fee,
                        ttl        = TTL,
                        deposit    = Deposit,
                        amount     = Amount,
                        gas        = Gas,
                        gas_price  = GasPrice,
                        call_data  = CallData}.

serialization_template(?CONTRACT_CREATE_TX_VSN) ->
    [ {owner, binary}
    , {nonce, int}
    , {code, binary}
    , {vm_version, int}
    , {fee, int}
    , {ttl, int}
    , {deposit, int}
    , {amount, int}
    , {gas, int}
    , {gas_price, int}
    , {call_data, binary}
    ].

for_client(#contract_create_tx{ owner      = OwnerPubKey,
                                nonce      = Nonce,
                                code       = Code,
                                vm_version = VmVersion,
                                fee        = Fee,
                                ttl        = TTL,
                                deposit    = Deposit,
                                amount     = Amount,
                                gas        = Gas,
                                gas_price  = GasPrice,
                                call_data  = CallData}) ->
    #{<<"data_schema">> => <<"ContractCreateTxObject">>, % swagger schema name
      <<"vsn">>         => version(),
      <<"owner">>       => dsdc_base58c:encode(account_pubkey, OwnerPubKey),
      <<"nonce">>       => Nonce,
      <<"code">>        => dsdct_utils:hex_bytes(Code),
      <<"vm_version">>  => dsdct_utils:hex_byte(VmVersion),
      <<"fee">>         => Fee,
      <<"ttl">>         => TTL,
      <<"deposit">>     => Deposit,
      <<"amount">>      => Amount,
      <<"gas">>         => Gas,
      <<"gas_price">>   => GasPrice,
      <<"call_data">>   => dsdct_utils:hex_bytes(CallData)}.

%%%===================================================================
%%% Internal functions

-spec version() -> non_neg_integer().
version() ->
    ?CONTRACT_CREATE_TX_VSN.

