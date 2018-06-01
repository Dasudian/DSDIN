-module(dsdcore_app_tests).

-include_lib("eunit/include/eunit.hrl").
-include("blocks.hrl").

persisted_valid_gen_block_test_() ->
    {foreach,
     fun() ->
             meck:new(dsdc_db, [passthrough]),
             meck:expect(dsdc_db, load_database, 0, ok),
             meck:new(dsdcore_sup, [passthrough]),
             meck:expect(dsdcore_sup, start_link, 0, {ok, pid}),
             lager:start(),
             ok
     end,
     fun(ok) ->
             ok = application:stop(lager),
             ok = application:stop(mnesia),
             meck:unload(dsdcore_sup),
             meck:unload(dsdc_db)
     end,
     [{"Check persisted genesis block",
       fun() ->
            meck:expect(dsdc_db, persisted_valid_genesis_block, 0, false),
            ?assertEqual({error, inconsistent_database},
                         dsdcore_app:start(normal, [])),

            meck:expect(dsdc_db, persisted_valid_genesis_block, 0, true),
            ?assertEqual({ok, pid}, dsdcore_app:start(normal, [])),
            ok
       end}
     ]}.

