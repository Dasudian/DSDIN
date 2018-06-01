
-module(dsdns_state_tree).

-include("dsdns.hrl").

%% API
-export([commit_to_db/1,
         delete_commitment/2,
         delete_name/2,
         empty/0,
         empty_with_backend/0,
         enter_commitment/2,
         enter_name/2,
         get_name/2,
         prune/2,
         lookup_commitment/2,
         lookup_name/2,
         root_hash/1]).

%% Export for test
-ifdef(TEST).
-export([ name_list/1
        , commitment_list/1
        ]).
-endif.

%%%===================================================================
%%% Types
%%%===================================================================

-type mkey() :: dsdns_commitments:id() | dsdns_names:id().
-type mvalue() :: dsdns_commitments:serialized() | dsdns_names:serialized().
-type nstree() :: dsdu_mtrees:mtree(mkey(), mvalue()).
-type commitment() :: dsdns_commitments:commitment().
-type name() :: dsdns_names:name().
-type cache() :: dsdu_mtrees:mtree(cache_key(), cache_value()).
-type cache_key() :: binary(). %% Sext encoded
-type cache_value() :: binary(). %% ?DUMMY_VAL
-type block_height() :: non_neg_integer().

-record(ns_tree, { mtree = dsdu_mtrees:empty() :: nstree()
                 , cache = dsdu_mtrees:empty() :: cache()
                 }).

-opaque tree() :: #ns_tree{}.

-export_type([tree/0]).

%%%===================================================================
%%% API
%%%===================================================================

-spec delete_commitment(binary(), tree()) -> tree().
delete_commitment(Id, Tree) ->
    MTree1 = dsdu_mtrees:delete(Id, Tree#ns_tree.mtree),
    Tree#ns_tree{mtree = MTree1}.

-spec delete_name(binary(), tree()) -> tree().
delete_name(Id, Tree) ->
    MTree1 = dsdu_mtrees:delete(Id, Tree#ns_tree.mtree),
    Tree#ns_tree{mtree = MTree1}.

-spec empty() -> tree().
empty() ->
    #ns_tree{}.

-spec empty_with_backend() -> tree().
empty_with_backend() ->
    MTree = dsdu_mtrees:empty_with_backend(dsdc_db_backends:ns_backend()),
    Cache = dsdu_mtrees:empty_with_backend(dsdc_db_backends:ns_cache_backend()),
    #ns_tree{mtree = MTree, cache = Cache}.

-spec prune(block_height(), tree()) -> tree().
prune(NextBlockHeight, #ns_tree{} = Tree) ->
    {Tree1, ExpiredActions} = int_prune(NextBlockHeight - 1, Tree),
    run_elapsed(ExpiredActions, Tree1, NextBlockHeight).

run_elapsed([], Tree, _) ->
    Tree;
run_elapsed([{Mod, Serialized}|Expired], Tree, Height) ->
    Entity = Mod:deserialize(Serialized),
    {ok, Tree1} = do_run_elapsed(Entity, Tree, Height),
    run_elapsed(Expired, Tree1, Height).

-spec enter_commitment(commitment(), tree()) -> tree().
enter_commitment(Commitment, Tree) ->
    Id = dsdns_commitments:id(Commitment),
    Serialized = dsdns_commitments:serialize(Commitment),
    Expiration = dsdns_commitments:expires(Commitment),
    %% TODO: consider two trees (names vs pre-claims/commitments)
    Cache1 = cache_push(Expiration, Id, dsdns_commitments, Tree#ns_tree.cache),
    MTree1 = dsdu_mtrees:insert(Id, Serialized, Tree#ns_tree.mtree),
    Tree#ns_tree{cache = Cache1, mtree = MTree1}.

-spec enter_name(name(), tree()) -> tree().
enter_name(Name, Tree) ->
    Id = dsdns_names:id(Name),
    Serialized = dsdns_names:serialize(Name),
    Expiration = dsdns_names:expires(Name),
    Cache1 = cache_push(Expiration, Id, dsdns_names, Tree#ns_tree.cache),
    MTree1 = dsdu_mtrees:enter(Id, Serialized, Tree#ns_tree.mtree),
    Tree#ns_tree{cache = Cache1, mtree = MTree1}.

-spec get_name(binary(), tree()) -> name().
get_name(Id, Tree) ->
    dsdns_names:deserialize(dsdu_mtrees:get(Id, Tree#ns_tree.mtree)).

-spec lookup_commitment(binary(), tree()) -> {value, commitment()} | none.
lookup_commitment(Id, Tree) ->
    case dsdu_mtrees:lookup(Id, Tree#ns_tree.mtree) of
        {value, Val} -> {value, dsdns_commitments:deserialize(Val)};
        none -> none
    end.

-spec lookup_name(binary(), tree()) -> {value, name()} | none.
lookup_name(Id, Tree) ->
    case dsdu_mtrees:lookup(Id, Tree#ns_tree.mtree) of
        {value, Val} -> {value, dsdns_names:deserialize(Val)};
        none -> none
    end.

-spec root_hash(tree()) -> {ok, dsdu_mtrees:root_hash()} | {error, empty}.
root_hash(#ns_tree{mtree = MTree}) ->
    dsdu_mtrees:root_hash(MTree).

-spec commit_to_db(tree()) -> tree().
commit_to_db(#ns_tree{mtree = MTree, cache = Cache} = Tree) ->
    Tree#ns_tree{mtree = dsdu_mtrees:commit_to_db(MTree),
                 cache = dsdu_mtrees:commit_to_db(Cache)
                }.

-ifdef(TEST).
-spec commitment_list(tree()) -> list(commitment()).
commitment_list(#ns_tree{mtree = Tree}) ->
    IsCommitment = fun(MaybeC) ->
                       try [dsdns_commitments:deserialize(MaybeC)]
                       catch _:_ -> [] end
                   end,
    [ C || {_, Val} <- dsdu_mtrees:to_list(Tree),
           C <- IsCommitment(Val) ].

-spec name_list(tree()) -> list(name()).
name_list(#ns_tree{mtree = Tree}) ->
    IsName = fun(MaybeC) ->
                 try [dsdns_names:deserialize(MaybeC)]
                 catch _:_ -> [] end
             end,
    [ C || {_, Val} <- dsdu_mtrees:to_list(Tree),
           C <- IsName(Val) ].
-endif.


%%%===================================================================
%%% Internal functions
%%%===================================================================

int_prune(NextBlockHeight, #ns_tree{cache = Cache, mtree = MTree} = Tree) ->
    {Cache1, Mtree1, ExpiredActions} = int_prune(cache_safe_peek(Cache), NextBlockHeight, Cache, MTree, []),
    {Tree#ns_tree{ cache = Cache1, mtree = Mtree1}, ExpiredActions}.

int_prune(none, _NextBlockHeight, Cache, MTree, ExpiredAcc) ->
    {Cache, MTree, lists:reverse(ExpiredAcc)};
int_prune({Height1,_Id,_Mod}, NextBlockHeight, Cache, MTree, ExpiredAcc) when NextBlockHeight < Height1 ->
    {Cache, MTree, lists:reverse(ExpiredAcc)};
int_prune({HeightLower, Id, Mod}, NextBlockHeight, Cache, MTree, ExpiredAcc) ->
    {{HeightLower, Id, Mod}, Cache1} = cache_pop(Cache),
    case dsdu_mtrees:lookup(Id, MTree) of
        {value, ExpiredAction} ->
            int_prune(cache_safe_peek(Cache1), NextBlockHeight, Cache1, MTree, [{Mod, ExpiredAction}|ExpiredAcc]);
        none ->
            int_prune(cache_safe_peek(Cache1), NextBlockHeight, Cache1, MTree, ExpiredAcc)
    end.

%% INFO: do_run_elapsed/3 implements 'expire' driven transitions:
%%
%%                   expire
%%       unclaimed <-------- revoked
%%           | ^              ^^
%%           | |              || expire
%%           | | expire       ||
%% pre-claim | |              ||  _
%%           | |       revoke || | | transfer
%%           v |              || | v
%%      pre-claimed -------> claimed
%%                   claim    | ^
%%                            | |
%%                             -
%%                           update
%%

do_run_elapsed(#name{expires = ExpirationBlockHeight}, NamesTree0, NextBlockHeight)
    when ExpirationBlockHeight /= NextBlockHeight-1 ->
    %% INFO: Do nothing. Name was updated and we triggered old cache event.
    {ok, NamesTree0};
do_run_elapsed(#name{hash = NameHash, status = claimed, expires = ExpirationBlockHeight},
               NamesTree0, NextBlockHeight) when ExpirationBlockHeight == NextBlockHeight-1 ->
    Name0 = dsdns_state_tree:get_name(NameHash, NamesTree0),
    TTL = dsdc_governance:name_protection_period(),
    Name1 = dsdns_names:revoke(Name0, TTL, ExpirationBlockHeight),
    NamesTree1 = dsdns_state_tree:enter_name(Name1, NamesTree0),
    {ok, NamesTree1};
do_run_elapsed(#name{hash = NameHash, status = revoked, expires = ExpirationBlockHeight},
    NamesTree0, NextBlockHeight) when ExpirationBlockHeight == NextBlockHeight-1  ->
    NamesTree1 = dsdns_state_tree:delete_name(NameHash, NamesTree0),
    {ok, NamesTree1};
do_run_elapsed(#commitment{hash = Hash}, NamesTree0, _Height) ->
    %% INFO: We delete in both cases when name is claimed or not claimed
    %%       when it expires
    NamesTree1 = dsdns_state_tree:delete_commitment(Hash, NamesTree0),
    {ok, NamesTree1}.

%%%===================================================================
%%% TTL Cache
%%%===================================================================
-define(DUMMY_VAL, <<0>>).

cache_push(Expires, Id, Mod, C) ->
    SExt = sext:encode({Expires, Id, Mod}),
    dsdu_mtrees:enter(SExt, ?DUMMY_VAL, C).

cache_safe_peek(C) ->
    case dsdu_mtrees:iterator_next(dsdu_mtrees:iterator(C)) of
        '$end_of_table' -> none;
        {Next, ?DUMMY_VAL, _Iter} -> sext:decode(Next)
    end.

cache_pop(C) ->
    case dsdu_mtrees:iterator_next(dsdu_mtrees:iterator(C)) of
        '$end_of_table' -> none;
        {Next,?DUMMY_VAL,_Iter} ->
            {sext:decode(Next), dsdu_mtrees:delete(Next, C)}
    end.
