
-module(dsdc_mining).

-export([mine/3]).

-ifdef(TEST).
-export([get_miner_account_balance/0]).
-endif.

-spec mine(binary(), dsdc_pow:sci_int(), dsdc_pow:nonce()) ->  dsdc_pow:pow_result().
mine(HeaderBin, Target, Nonce) ->
    dsdc_pow_cuckoo:generate(HeaderBin, Target, Nonce).

-ifdef(TEST).
-spec get_miner_account_balance() -> {ok, non_neg_integer()} |
                                     {error, account_not_found}.
get_miner_account_balance() ->
    {ok, Pubkey} = dsdc_keys:pubkey(),
    case dsdc_chain:get_account(Pubkey) of
        {value, A} ->
            {ok, dsdc_accounts:balance(A)};
        none ->
            {error, account_not_found}
    end.
-endif.

