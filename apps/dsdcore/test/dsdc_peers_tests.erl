
-module(dsdc_peers_tests).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

-define(A_KEY, <<32,85,25,13,96,60,236,26,225,111,56,107,78,47,70,220,104,39,95,162,186,6,196,171,235,241,179,126,68,226,208,123>>).
-define(ANOTHER_KEY, <<33,85,25,13,96,60,236,26,225,111,56,107,78,47,70,220,104,39,95,162,186,6,196,171,235,241,179,126,68,226,208,123>>).

someone() ->
    #{ host => <<"someone.somewhere">>, port => 1337, pubkey => ?A_KEY }.

someoneelse() ->
    #{ host => <<"someoneelse.somewhereelse">>, port => 1337, pubkey => ?ANOTHER_KEY }.

localhost() ->
    localhost(800).

localhost(Port) ->
    #{ host => <<"localhost">>, port => Port, pubkey => <<Port:16, 0:(30*8)>> }.


all_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     [{"Add a peer",
       fun() ->
               ?assertEqual(ok, dsdc_peers:add_and_ping_peers([someone()])),
               timer:sleep(10) %% Let the someone connect
       end},
      {"Get a random peer (from list of 1)",
       fun() ->
               [Peer] = dsdc_peers:get_random(1),
               ?assertEqual(someone(), Peer)
       end},
      {"Add a second peer",
       fun() ->
               ?assertEqual(ok, dsdc_peers:add_and_ping_peers([someoneelse()])),
               timer:sleep(10)
       end},
      {"All and randomly getting peers",
       fun() ->
               ?assertEqual(2, length(dsdc_peers:all())),
               [Peer] = dsdc_peers:get_random(1),
               ?assert(lists:member(Peer, [someone(), someoneelse()])),
               ?assertEqual([someoneelse()], dsdc_peers:get_random(2, [dsdc_peers:peer_id(someone())]))
       end},
      {"Remove a peer",
       fun() ->
               ?assertEqual(ok, dsdc_peers:remove(someone())),
               ?assertEqual(1, length(dsdc_peers:all()))
       end},
      {"Remove all",
       fun do_remove_all/0},
      {"Random peer from nothing",
       fun() ->
               ?assertEqual([], dsdc_peers:get_random(2))
       end},
      {"Add peer",
       fun() ->
               ok = dsdc_peers:add(localhost()),
               ?assertEqual([localhost()], dsdc_peers:all())
       end},
      {"Get random N",
       fun() ->
               do_remove_all(),
               ok = dsdc_peers:add_and_ping_peers([localhost(N) || N <- lists:seq(900, 910)]),
               timer:sleep(25),
               L1 = dsdc_peers:get_random(5),
               5 = length(L1)
       end}

     ]
    }.

do_remove_all() ->
    [dsdc_peers:remove(P) || P <- dsdc_peers:all()],
    [] = dsdc_peers:all(),
    ok.

setup() ->
    application:ensure_started(crypto),
    application:ensure_started(gproc),

    meck:new(dsdc_keys, [passthrough]),
    meck:expect(dsdc_keys, peer_privkey, fun() -> {ok, <<0:(32*8)>>} end),
    meck:expect(dsdc_keys, peer_pubkey, fun() -> {ok, <<0:(32*8)>>} end),

    meck:new(dsdc_peer_connection, [passthrough]),
    meck:expect(dsdc_peer_connection, connect,
        fun(PeerInfo = #{ r_pubkey := PK }) ->
            Pid = spawn(fun() ->
                    timer:sleep(1),
                    dsdc_peers:connect_peer(dsdc_peers:peer_id(PeerInfo#{ pubkey := PK }), self()),
                    timer:sleep(500)
                  end),
            {ok, Pid}
        end),

    dsdc_test_utils:fake_start_dsdhttp(), %% tricking dsdc_peers
    dsdc_peers:start_link(),
    ok.

teardown(_) ->
    gen_server:stop(dsdc_peers),
    meck:unload(dsdc_keys),
    meck:unload(dsdc_peer_connection),
    application:stop(gproc),
    crypto:stop().

-endif.
