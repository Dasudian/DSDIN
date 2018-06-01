-module(dsdc_db_tests).

-include_lib("eunit/include/eunit.hrl").
-include("blocks.hrl").

-define(compareBlockResults(B1, B2),
        ?assertEqual(dsdc_blocks:serialize_to_map(B1),
                     dsdc_blocks:serialize_to_map(B2))).

block_hash(B) ->
    {ok, Hash} = dsdc_headers:hash_header(dsdc_blocks:to_header(B)),
    Hash.

kill_and_restart_conductor() ->
    %% Stop server
    ok = dsdc_conductor:stop(),
    %% check that it is dead.
    dead =
        try sys:get_status(dsdc_conductor)
        catch exit:{noproc, _} -> dead
        end,
    %% Restart server
    {ok, _} = dsdc_conductor:start_link([{autostart, false}]),
    server_up = wait_for_conductor(),
    ok.

wait_for_conductor() ->
    try sys:get_status(dsdc_conductor),
        server_up
    catch exit:{noproc, _} ->
        timer:sleep(10),
        wait_for_conductor()
    end.

write_chain_test_() ->
    {foreach,
     fun() ->
             ok = application:ensure_started(gproc),
             dsdc_test_utils:start_chain_db(),
             meck:new(dsdc_pow_cuckoo, [passthrough]),
             meck:expect(dsdc_pow_cuckoo, verify, fun(_, _, _, _) -> true end),
             meck:new(dsdc_events, [passthrough]),
             meck:expect(dsdc_events, publish, fun(_, _) -> ok end),
             dsdc_test_utils:mock_genesis(),
             TmpDir = dsdc_test_utils:dsdc_keys_setup(),
             {ok, _} = dsdc_tx_pool:start_link(),
             {ok, _} = dsdc_conductor:start_link([{autostart, false}]),
             TmpDir
     end,
     fun(TmpDir) ->
             ok = dsdc_conductor:stop(),
             ok = dsdc_tx_pool:stop(),
             ok = application:stop(gproc),
             meck:unload(dsdc_pow_cuckoo),
             meck:unload(dsdc_events),
             dsdc_test_utils:unmock_genesis(),
             dsdc_test_utils:stop_chain_db(),
             dsdc_test_utils:dsdc_keys_cleanup(TmpDir)
     end,
     [{"Write a block to chain and read it back.",
       fun() ->
               GB = dsdc_test_utils:genesis_block(),
               ok = dsdc_conductor:post_block(GB),

               Hash = block_hash(GB),

               Block = dsdc_db:get_block(Hash),

               %% Check block apart from state trees.
               ?compareBlockResults(GB, Block),

               %% Genesis should be top block
               TopBlockHash = dsdc_db:get_top_block_hash(),
               ?assertEqual(Hash, TopBlockHash),

               ok
       end},
      {"Build chain with genesis block plus 2 blocks",
       fun() ->
               [GB, B1, B2] = dsdc_test_utils:gen_blocks_only_chain(3),

               GHash = block_hash(GB),
               Block = dsdc_db:get_block(GHash),

               %% Check block apart from state trees.
               ?compareBlockResults(GB, Block),

               %% Genesis should be top block
               TopBlockHash = dsdc_db:get_top_block_hash(),
               ?assertEqual(GHash, TopBlockHash),

               %% Add one block
               ?assertEqual(ok, dsdc_conductor:post_block(B2)),

               %% GB should still be top block
               NewTopBlockHash = dsdc_db:get_top_block_hash(),
               ?assertEqual(GHash, NewTopBlockHash),

               %% Add missing block
               ?assertEqual(ok, dsdc_conductor:post_block(B1)),

               %% Now B2 should be the top block
               LastTopBlockHash = dsdc_db:get_top_block_hash(),
               B2Hash = block_hash(B2),
               ?assertEqual(B2Hash, LastTopBlockHash),

               ok
       end}
     ]}.


restart_test_() ->
    {foreach,
     fun() ->
             ok = application:ensure_started(gproc),
             TmpDir = dsdc_test_utils:dsdc_keys_setup(),
             dsdc_test_utils:start_chain_db(),
             meck:new(dsdc_events, [passthrough]),
             meck:expect(dsdc_events, publish, fun(_, _) -> ok end),
             meck:new(dsdc_pow_cuckoo, [passthrough]),
             meck:expect(dsdc_pow_cuckoo, verify, fun(_, _, _, _) -> true end),
             dsdc_test_utils:mock_genesis(),
             {ok, _} = dsdc_tx_pool:start_link(),
             {ok, _} = dsdc_conductor:start_link([{autostart, false}]),
             TmpDir
     end,
     fun(TmpDir) ->
             ok = dsdc_tx_pool:stop(),
             meck:unload(dsdc_pow_cuckoo),
             meck:unload(dsdc_events),
             ok = application:stop(gproc),
             dsdc_test_utils:unmock_genesis(),
             dsdc_test_utils:stop_chain_db(),
             dsdc_test_utils:dsdc_keys_cleanup(TmpDir)
     end,
     [{"Build chain, then kill server, check that chain is read back.",
       fun() ->
               [GB, B1, B2] = dsdc_test_utils:gen_blocks_only_chain(3),
               ?assertEqual({ok, GB}, dsdc_chain:get_block_by_height(0)),
               ?assertEqual(ok, dsdc_conductor:post_block(B1)),
               ?assertEqual(ok, dsdc_conductor:post_block(B2)),
               %% Now B2 should be the top block
               TopBlockHash = dsdc_db:get_top_block_hash(),
               B2Hash = block_hash(B2),
               ?assertEqual(B2Hash, TopBlockHash),
               ChainTop1 = dsdc_chain:top_block(),
               ?compareBlockResults(B2, ChainTop1),

               %% Check the state trees from persistence
               {ok, ChainTop1Hash} =
                   dsdc_blocks:hash_internal_representation(ChainTop1),
               {ok, ChainTop1State} =
                   dsdc_chain:get_block_state(ChainTop1Hash),
               ?assertEqual(ChainTop1State,
                            dsdc_db:get_block_state(TopBlockHash)),

               %% Kill chain server

               kill_and_restart_conductor(),
               NewTopBlockHash = dsdc_db:get_top_block_hash(),
               ?assertEqual(B2Hash, NewTopBlockHash),

               ChainTop2 = dsdc_chain:top_block(),
               ?compareBlockResults(B2, ChainTop2),

               %% Compare the trees after restart
               %% Check the state trees from persistence
               {ok, ChainTop2Hash} =
                   dsdc_blocks:hash_internal_representation(ChainTop2),
               {ok, ChainTop2State} =
                   dsdc_chain:get_block_state(ChainTop2Hash),
               ?assertEqual(ChainTop1State,
                            ChainTop2State),

               ok
       end}
     ]}.

persisted_valid_gen_block_test_() ->
    {foreach,
     fun() ->
             TmpDir = dsdc_test_utils:dsdc_keys_setup(),
             dsdc_test_utils:mock_genesis(),
             meck:new(dsdc_db, [passthrough]),
             TmpDir
     end,
     fun(TmpDir) ->
             dsdc_test_utils:unmock_genesis(),
             dsdc_test_utils:dsdc_keys_cleanup(TmpDir),
             meck:unload(dsdc_db),
             application:set_env(dsdcore, persist, false)
     end,
     [{"Check persisted validation of genesis block, persistence OFF",
       fun() ->
            application:set_env(dsdcore, persist, false),
            ?assertEqual(true, dsdc_db:persisted_valid_genesis_block())
       end},
      {"Check persisted validation of genesis block, persistence ON",
       fun() ->
            application:set_env(dsdcore, persist, true),

            meck:expect(dsdc_db, get_genesis_hash, 0, undefined),
            ?assertEqual(true, dsdc_db:persisted_valid_genesis_block()),

            {ok, ExpectedGH} = dsdc_headers:hash_header(dsdc_block_genesis:genesis_header()),

            meck:expect(dsdc_db, get_genesis_hash, 0, ExpectedGH),
            ?assertEqual(true, dsdc_db:persisted_valid_genesis_block()),

            meck:expect(dsdc_db, get_genesis_hash, 0, <<"invalid genesis block hash">>),
            ?assertEqual(false, dsdc_db:persisted_valid_genesis_block()),
            ok
       end}
     ]}.

