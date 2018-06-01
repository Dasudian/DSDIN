%%%=============================================================================
%%% @copyright 2018, Dasudian Technologies
%%% @doc
%%%    Dummy implementation of the dsdvm_chain_api. Doesn't keep any state. To
%%%    use when testing contracts that don't interact with the chain.
%%% @end
%%%=============================================================================

-module(dsdvm_dummy_chain).

-behaviour(dsdvm_chain_api).

-export([new_state/0]).

%% dsdvm_chain_api callbacks
-export([get_balance/2,
	 get_store/1,
	 set_store/2,
         spend/3,
         call_contract/6]).

new_state() -> no_state.

get_balance(_, _S)              -> 0.
get_store(_)                    -> #{}.
set_store(_,S)                  -> S.
spend(_Recipient, _Amount, _S)  -> {error, cant_spend_with_dummy_chain}.
call_contract(_, _, _, _, _, _) -> {error, cant_call_contracts_with_dummy_chain}.

