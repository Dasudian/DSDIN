
-module(dsdc_accounts_trees).

%% API - similar to OTP `gb_trees` module
-export([empty/0,
         empty_with_backend/0,
         get/2,
         lookup/2,
         enter/2]).

%% API - Merkle tree
-export([root_hash/1,
         commit_to_db/1
        ]).

%% API - Proof of inclusion
-export([ add_poi/3
        , verify_poi/3
        ]).

%% API - misc
-export([get_all_accounts_balances/1]).

-export_type([tree/0]).

-type key() :: dsdc_keys:pubkey().
-type value() :: dsdc_accounts:deterministic_account_binary_with_pubkey().
-opaque tree() :: dsdu_mtrees:mtree(key(), value()).

%%%===================================================================
%%% API - similar to OTP `gb_trees` module
%%%===================================================================
-spec empty() -> tree().
empty() ->
    dsdu_mtrees:empty().

-spec empty_with_backend() -> tree().
empty_with_backend() ->
    dsdu_mtrees:empty_with_backend(dsdc_db_backends:accounts_backend()).

-spec get(dsdc_keys:pubkey(), tree()) -> dsdc_accounts:account().
get(Pubkey, Tree) ->
    Account = dsdc_accounts:deserialize(dsdu_mtrees:get(Pubkey, Tree)),
    Pubkey  = dsdc_accounts:pubkey(Account), %% Hardcoded expectation.
    Account.

-spec lookup(dsdc_keys:pubkey(), tree()) -> none | {value, dsdc_accounts:account()}.
lookup(Pubkey, Tree) ->
    case dsdu_mtrees:lookup(Pubkey, Tree) of
        none ->
            none;
        {value, SerializedAccount} ->
            Account = dsdc_accounts:deserialize(SerializedAccount),
            Pubkey  = dsdc_accounts:pubkey(Account), %% Hardcoded expectation.
            {value, Account}
    end.

-spec enter(dsdc_accounts:account(), tree()) -> tree().
enter(Account, Tree) ->
    dsdu_mtrees:enter(key(Account), value(Account), Tree).

%%%===================================================================
%%% API - Merkle tree
%%%===================================================================

-spec root_hash(tree()) -> {ok, dsdu_mtrees:root_hash()} | {error, empty}.
root_hash(Tree) ->
    dsdu_mtrees:root_hash(Tree).

-spec add_poi(dsdc_keys:pubkey(), tree(), dsdc_poi:poi()) ->
                     {'ok', binary(), dsdc_poi:poi()}
                         | {'error', 'not_present' | 'wrong_root_hash'}.
add_poi(Pubkey, Tree, Poi) ->
    dsdc_poi:add_poi(Pubkey, Tree, Poi).

-spec verify_poi(dsdc_keys:pubkey(), binary(), dsdc_poi:poi()) ->
                        'ok' | {'error', term()}.
verify_poi(AccountKey, SerializedAccount, Poi) ->
    dsdc_poi:verify(AccountKey, SerializedAccount, Poi).

-spec commit_to_db(tree()) -> tree().
commit_to_db(Tree) ->
    dsdu_mtrees:commit_to_db(Tree).

%%%===================================================================
%%% API - misc
%%%===================================================================

-spec get_all_accounts_balances(tree()) -> [{dsdc_keys:pubkey(), non_neg_integer()}].
get_all_accounts_balances(AccountsTree) ->
    AccountsDump = dsdu_mtrees:to_list(AccountsTree),
    lists:foldl(
      fun({Pubkey, SerializedAccount}, Acc) ->
              Account = dsdc_accounts:deserialize(SerializedAccount),
              [{Pubkey, dsdc_accounts:balance(Account)} | Acc]
      end, [], AccountsDump).

%%%===================================================================
%%% Internal functions
%%%===================================================================

key(A) ->
    dsdc_accounts:pubkey(A).

value(A) ->
    dsdc_accounts:serialize(A).
