
-module(dsdc_keys_tests).


-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

all_test_() ->
    {foreach,
     fun() ->
             dsdc_test_utils:dsdc_keys_setup()
     end,
     fun(TmpKeysDir) ->
             dsdc_test_utils:dsdc_keys_cleanup(TmpKeysDir)
     end,
     [fun(_) ->
              [{"Sign spend transaction",
                fun() ->
                        {ok, PubKey} = dsdc_keys:pubkey(),
                        #{ public := RecipientPubkey } = enacl:sign_keypair(),
                        {ok, Tx} =
                            dsdc_spend_tx:new(#{sender => PubKey,
                                               recipient => RecipientPubkey,
                                               amount => 10,
                                               fee => 2,
                                               ttl => 100,
                                               nonce => 3,
                                               payload => <<"">>}),
                        {ok, SignedTx} = dsdc_keys:sign(Tx),
                        ?assertEqual(Tx, dsdtx_sign:tx(SignedTx))
                end},
               {"Keys validation (positive case)",
                fun() ->
                        #{ public := Pubkey, secret := PrivKey} = enacl:sign_keypair(),
                        ValidPair = dsdc_keys:check_sign_keys(Pubkey, PrivKey),
                        ?assertEqual(true, ValidPair)
                end},
               {"Keys validation (negative case)",
                fun() ->
                        Pubkey = <<42:32/unit:8>>,
                        Privkey = <<42:64/unit:8>>,
                        ValidPair = dsdc_keys:check_sign_keys(Pubkey, Privkey),
                        ?assertEqual(false, ValidPair)
                end},
                {"Peer key validation (positive case)",
                fun() ->
                        KeyPair = enoise_keypair:new(dh25519),
                        Pubkey  = enoise_keypair:pubkey(KeyPair),
                        Privkey = enoise_keypair:seckey(KeyPair),
                        ValidPair = dsdc_keys:check_peer_keys(Pubkey, Privkey),
                        ?assertEqual(true, ValidPair)
                end},
               {"Peer key validation (negative case)",
                fun() ->
                        Pubkey = <<0:(32*8)>>,
                        Privkey = <<0:(32*8)>>,
                        ValidPair = dsdc_keys:check_peer_keys(Pubkey, Privkey),
                        ?assertEqual(false, ValidPair)
                end}]
      end]}.

start_test_() ->
    TF =
        fun() ->
                ?_test(
                   dsdc_test_utils:dsdc_keys_bare_cleanup(
                     dsdc_test_utils:dsdc_keys_bare_setup()))
        end,
    {setup,
     fun() -> ok = application:ensure_started(crypto) end,
     fun(_) -> ok = application:stop(crypto) end,
     lazy_gen(100, TF)}.

lazy_gen(N, TF) ->
    {generator,
     fun () ->
             if N > 0 ->
                     [TF()
                      | lazy_gen(N-1, TF)];
                true ->
                     []
             end
     end}.

-endif.
