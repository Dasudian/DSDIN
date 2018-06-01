%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Dasudian Technologies
%%% @doc Basic tests for dsdvm
%%% @end
%%%-------------------------------------------------------------------

-module(dsdvm_store_tests).

-include_lib("eunit/include/eunit.hrl").

init_test() ->
    #{ storage := #{} }  = dsdvm_eeevm_store:init(#{}, #{ storage => undef}),
    A = 1 bsl 255,
    V = 2,
    K = <<A:256>>,
    Val = <<V:256>>,
    #{ storage := #{A := V} }  = dsdvm_eeevm_store:init(#{K => Val}, #{storage => #{}}).

to_binary_test() ->
    A = 1 bsl 255,
    V = 2,
    K = <<A:256>>,
    Val = <<V:256>>,
    #{K := <<V>>} = dsdvm_eeevm_store:to_binary(
                    dsdvm_eeevm_store:init(#{K => Val}, #{storage => #{}})).


store_test() ->
    State0 = dsdvm_eeevm_store:init(#{}, #{ storage => undef}),
    State1 = dsdvm_eeevm_store:store(32, 17, State0),
    #{ <<32>> := <<17>> } = dsdvm_eeevm_store:to_binary(State1).

load_test() ->
    State0 = dsdvm_eeevm_store:init(#{ <<32>> => <<17>>}, #{}),
    17 = dsdvm_eeevm_store:load(32, State0).
