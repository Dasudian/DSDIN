%%%-------------------------------------------------------------------
%%% @copyright (C) 2018, Dasudian Technologies
%%% @doc
%%%     Linter for Varna.
%%% @end
%%%
%%%-------------------------------------------------------------------

-module(dsdva_lint).

-export([contract/1]).

-record(lint, {contract=[],                     %Contract name
               state=[],                        %State definition.
               funcs=[],                        %Defined functions
               errors=[],                       %Error
               warnings={}                      %Warnings
              }).

%% contract(Contract) -> {ok,Warnings} | {error,Errors,Warnings}.

contract(_Form) ->
    {ok,[]}.
