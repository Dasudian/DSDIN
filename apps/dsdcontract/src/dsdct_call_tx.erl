
-module(dsdct_call_tx).

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
-export([caller/1,
         contract/1,
         vm_version/1,
         amount/1,
         gas/1,
         gas_price/1,
         call_data/1,
         call_stack/1]).

-define(CONTRACT_CALL_TX_VSN, 1).
-define(CONTRACT_CALL_TX_TYPE, contract_call_tx).
-define(CONTRACT_CALL_TX_FEE, 2).

-opaque tx() :: #contract_call_tx{}.

-export_type([tx/0]).

-spec new(map()) -> {ok, dsdtx:tx()}.
new(#{caller     := CallerPubKey,
      nonce      := Nonce,
      contract   := Contract,
      vm_version := VmVersion,
      fee        := Fee,
      ttl        := TTL,
      amount     := Amount,
      gas        := Gas,
      gas_price  := GasPrice,
      call_data  := CallData} = Args) ->
    CallStack = maps:get(call_stack, Args, []),
    Tx = #contract_call_tx{caller     = CallerPubKey,
                           nonce      = Nonce,
                           contract   = Contract,
                           vm_version = VmVersion,
                           fee        = Fee,
                           ttl        = TTL,
                           amount     = Amount,
                           gas        = Gas,
                           gas_price  = GasPrice,
                           call_data  = CallData,
                           call_stack = CallStack},
    {ok, dsdtx:new(?MODULE, Tx)}.

-spec type() -> atom().
type() ->
    ?CONTRACT_CALL_TX_TYPE.

-spec fee(tx()) -> integer().
fee(#contract_call_tx{fee = F}) ->
    F.

-spec ttl(tx()) -> dsdc_blocks:height().
ttl(#contract_call_tx{ttl = TTL}) ->
    TTL.

-spec nonce(tx()) -> non_neg_integer().
nonce(#contract_call_tx{nonce = Nonce}) ->
    Nonce.

-spec origin(tx()) -> dsdc_keys:pubkey().
origin(#contract_call_tx{caller = CallerPubKey}) ->
    CallerPubKey.

%% CallerAccount should exist, and have enough funds for the fee + gas cost
%% Contract should exist and its vm_version should match the one in the call.
-spec check(tx(), dsdtx:tx_context(), dsdc_trees:trees(), dsdc_blocks:height(), non_neg_integer()) -> {ok, dsdc_trees:trees()} | {error, term()}.
check(#contract_call_tx{caller = CallerPubKey, nonce = Nonce,
                        fee = Fee, amount = Value,
                        gas = GasLimit, gas_price = GasPrice,
                        call_stack = CallStack
                       } = CallTx, Context, Trees, Height, _ConsensusVersion) ->
    Checks =
        case Context of
            dsdtx_transaction ->
                RequiredAmount = Fee + GasLimit * GasPrice + Value,
                [fun() -> dsdtx_utils:check_account(CallerPubKey, Trees, Nonce, RequiredAmount) end,
                 fun() -> check_call(CallTx, Trees, Height) end,
                 fun() -> dsdct_utils:check(CallStack == [], nonempty_call_stack) end];
            dsdtx_contract ->
                [fun() -> dsdct_utils:check_balance(CallerPubKey, Trees, Value) end,
                 fun() -> check_call(CallTx, Trees, Height) end]
        end,

    case dsdu_validation:run(Checks) of
        ok              -> {ok, Trees};
        {error, Reason} -> {error, Reason}
    end.

-spec accounts(tx()) -> [dsdc_keys:pubkey()].
accounts(Tx) ->
    [caller(Tx)].

-spec signers(tx(), dsdc_trees:trees()) -> {ok, [dsdc_keys:pubkey()]}.
signers(Tx, _) ->
    {ok, [caller(Tx)]}.

-spec process(tx(), dsdtx:tx_context(), dsdc_trees:trees(), dsdc_blocks:height(), non_neg_integer()) -> {ok, dsdc_trees:trees()}.
process(#contract_call_tx{caller = CallerPubKey, contract = CalleePubKey, nonce = Nonce,
                          fee = Fee, gas =_Gas, gas_price = GasPrice, amount = Value
                         } = CallTx, Context, Trees1, Height, ConsensusVersion) ->

    %% Transfer the attached funds to the callee (before calling the contract!)
    Trees2 = spend(CallerPubKey, CalleePubKey, Value,
                   Nonce, Context, Height, Trees1, ConsensusVersion),

    %% Create the call.
    Call0 = dsdct_call:new(dsdct_call_tx:caller(CallTx),
			  dsdct_call_tx:nonce(CallTx),
			  dsdct_call_tx:contract(CallTx),
			  Height),

    %% Run the contract code. Also computes the amount of gas left and updates
    %% the call object.
    {Call, Trees3} = run_contract(CallTx, Call0, Height, Trees2),

    %% Charge the fee and the used gas to the caller (not if called from another contract!)
    AccountsTree1 = dsdc_trees:accounts(Trees3),
    AccountsTree2 =
        case Context of
            dsdtx_contract    ->
                AccountsTree1;
            dsdtx_transaction ->
                %% When calling from the top-level we charge Fee and Gas as well.
                GasCost       = dsdct_call:gas_used(Call) * GasPrice,
                Amount        = Fee + GasCost,
                Caller2       = dsdc_accounts_trees:get(CallerPubKey, AccountsTree1),
                {ok, Caller3} = dsdc_accounts:spend(Caller2, Amount, Nonce),
                dsdc_accounts_trees:enter(Caller3, AccountsTree1)
        end,
    Trees4 = dsdc_trees:set_accounts(Trees3, AccountsTree2),

    %% Insert the call into the state tree. This is mainly to remember what the
    %% return value was so that the caller can access it easily.
    %% Each block starts with an empty calls tree.
    CallsTree0 = dsdc_trees:calls(Trees4),
    CallsTree1 = dsdct_call_state_tree:insert_call(Call, CallsTree0),
    Trees5 = dsdc_trees:set_calls(Trees4, CallsTree1),
    {ok, Trees5}.

spend(CallerPubKey, CalleePubKey, Value, Nonce,_Context, Height, Trees,
      ConsensusVersion) ->
    {ok, SpendTx} = dsdc_spend_tx:new(#{ sender => CallerPubKey
                                , recipient => CalleePubKey
                                , amount => Value
                                , fee => 0
                                , ttl => Height
                                , nonce => Nonce
                                , payload => <<>>}),
    {ok, Trees1} =
        dsdtx:check_from_contract(SpendTx, Trees, Height, ConsensusVersion),
    {ok, Trees2} =
        dsdtx:process_from_contract(SpendTx, Trees1, Height, ConsensusVersion),
    Trees2.

run_contract(#contract_call_tx{	caller = Caller
			      , nonce  = _Nonce
			      , contract = ContractPubKey
			      , vm_version = VmVersion
			      , amount     = Amount
			      , gas        = Gas
			      , gas_price  = GasPrice
			      , call_data  = CallData
			      , call_stack = CallStack
			      } = _Tx, Call, Height, Trees) ->
    ContractsTree = dsdc_trees:contracts(Trees),
    Contract      = dsdct_state_tree:get_contract(ContractPubKey, ContractsTree),
    Code          = dsdct_contracts:code(Contract),
    CallDef = #{ caller     => Caller
	       , contract   => ContractPubKey
	       , gas        => Gas
	       , gas_price  => GasPrice
	       , call_data  => CallData
	       , amount     => Amount
	       , call_stack => CallStack
	       , code       => Code
	       , call       => Call
	       , height     => Height
	       , trees      => Trees
	       },
    dsdct_dispatch:run(VmVersion, CallDef).

serialize(#contract_call_tx{caller     = CallerPubKey,
                            nonce      = Nonce,
                            contract   = ContractPubKey,
                            vm_version = VmVersion,
                            fee        = Fee,
                            ttl        = TTL,
                            amount     = Amount,
                            gas        = Gas,
                            gas_price  = GasPrice,
                            call_data  = CallData}) ->
    %% Note that the call_stack is not serialized. This is ok since we don't
    %% serialize transactions originating from contract execution, and for
    %% top-level transactions the call_stack is always empty.
    {version(),
     [ {caller, CallerPubKey}
     , {nonce, Nonce}
     , {contract, ContractPubKey}
     , {vm_version, VmVersion}
     , {fee, Fee}
     , {ttl, TTL}
     , {amount, Amount}
     , {gas, Gas}
     , {gas_price, GasPrice}
     , {call_data, CallData}
     ]}.

deserialize(?CONTRACT_CALL_TX_VSN,
            [ {caller, CallerPubKey}
            , {nonce, Nonce}
            , {contract, ContractPubKey}
            , {vm_version, VmVersion}
            , {fee, Fee}
            , {ttl, TTL}
            , {amount, Amount}
            , {gas, Gas}
            , {gas_price, GasPrice}
            , {call_data, CallData}]) ->
    #contract_call_tx{caller     = CallerPubKey,
                      nonce      = Nonce,
                      contract   = ContractPubKey,
                      vm_version = VmVersion,
                      fee        = Fee,
                      ttl        = TTL,
                      amount     = Amount,
                      gas        = Gas,
                      gas_price  = GasPrice,
                      call_data  = CallData}.

serialization_template(?CONTRACT_CALL_TX_VSN) ->
    [ {caller, binary}
    , {nonce, int}
    , {contract, binary}
    , {vm_version, int}
    , {fee, int}
    , {ttl, int}
    , {amount, int}
    , {gas, int}
    , {gas_price, int}
    , {call_data, binary}
    ].

-spec version() -> non_neg_integer().
version() ->
    ?CONTRACT_CALL_TX_VSN.

for_client(#contract_call_tx{caller     = CallerPubKey,
                             nonce      = Nonce,
                             contract   = ContractPubKey,
                             vm_version = VmVersion,
                             fee        = Fee,
                             ttl        = TTL,
                             amount     = Amount,
                             gas        = Gas,
                             gas_price  = GasPrice,
                             call_data  = CallData}) ->
    #{<<"data_schema">> => <<"ContractCallTxObject">>, % swagger schema name
      <<"vsn">>         => version(),
      <<"caller">>      => dsdc_base58c:encode(account_pubkey, CallerPubKey),
      <<"nonce">>       => Nonce,
      <<"contract">>    => dsdc_base58c:encode(contract_pubkey, ContractPubKey),
      <<"vm_version">>  => dsdct_utils:hex_byte(VmVersion),
      <<"fee">>         => Fee,
      <<"ttl">>         => TTL,
      <<"amount">>      => Amount,
      <<"gas">>         => Gas,
      <<"gas_price">>   => GasPrice,
      <<"call_data">>   => dsdct_utils:hex_bytes(CallData)}.

%% -- Getters ----------------------------------------------------------------

-spec caller(tx()) -> dsdc_keys:pubkey().
caller(C) -> C#contract_call_tx.caller.

-spec contract(tx()) -> dsdc_keys:pubkey().
contract(C) -> C#contract_call_tx.contract.

-spec vm_version(tx()) -> dsdct_contracts:vm_version().
vm_version(C) -> C#contract_call_tx.vm_version.

-spec amount(tx()) -> dsdct_contracts:amount().
amount(C) -> C#contract_call_tx.amount.

-spec gas(tx()) -> dsdct_contracts:amount().
gas(C) -> C#contract_call_tx.gas.

-spec gas_price(tx()) -> dsdct_contracts:amount().
gas_price(C) -> C#contract_call_tx.gas_price.

-spec call_data(tx()) -> binary().
call_data(C) -> C#contract_call_tx.call_data.

-spec call_stack(tx()) -> [non_neg_integer()].
call_stack(C) -> C#contract_call_tx.call_stack.

%% -- Local functions  -------------------------------------------------------

%% Check that the contract exists and has the right VM version.
check_call(#contract_call_tx{ contract   = ContractPubKey,
                              vm_version = VmVersion,
                              amount     = Value},
               Trees, _Height) ->
    ContractsTree = dsdc_trees:contracts(Trees),
    %% Dialyzer, in its infinite wisdom, complains if it thinks we're checking
    %% that something of type non_neg_integer() is negative. Since Dialyzer
    %% doesn't _actually_ guarantee that this isn't the case, we do need the
    %% check and this is the least convoluted way of writing it that Dialyzer's
    %% static analysis cannot see through.
    NegativeAmount = -Value > 0,
    case dsdct_state_tree:lookup_contract(ContractPubKey, ContractsTree) of
        _ when NegativeAmount -> {error, negative_amount};
        {value, C} ->
            case dsdct_contracts:vm_version(C) == VmVersion of
                true  -> ok;
                false -> {error, wrong_vm_version}
            end;
        none -> {error, contract_does_not_exist}
    end.
