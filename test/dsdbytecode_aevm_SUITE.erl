-module(dsdbytecode_dsdvm_SUITE).

%% common_test exports
-export(
   [
    all/0
   ]).

%% test case exports
-export(
   [ execute_identy_fun_from_file/1
   ]).

-include("apps/dsdcontract/src/dsdcontract.hrl").
-include_lib("common_test/include/ct.hrl").

all() ->
    [ execute_identy_fun_from_file ].

execute_identy_fun_from_file(_Cfg) ->
    CodeDir = code:lib_dir(dsdbytecode, test),
    FileName = filename:join(CodeDir, "asm_code/identity.dsdsm"),
    Code = dsdb_asm:file(FileName, []),
    ChainState = dsdvm_dummy_chain:new_state(),
    {ok, Res} =
        dsdvm_eeevm:eval(
          dsdvm_eeevm_state:init(
            #{ exec => #{ code => Code,
                          address => 90120,
                          caller => 0,
                          data => <<0:256, 42:256>>,
                          gas => 1000000,
                          gasPrice => 1,
                          origin => 0,
                          value => 0 },
               env => #{currentCoinbase => 0,
                        currentDifficulty => 0,
                        currentGasLimit => 10000,
                        currentNumber => 0,
                        currentTimestamp => 0,
                        chainState => ChainState,
                        chainAPI => dsdvm_dummy_chain,
                        vm_version => ?AEVM_01_Solidity_01},
               pre => #{}},
            #{trace => false})
         ),
    #{ out := << RetVal:256 >> } = Res,
    42 = RetVal,
    ok.

