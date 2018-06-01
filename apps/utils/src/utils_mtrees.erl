%%%-------------------------------------------------------------------
%%% @copyright (C) 2018, Dasudian Technologies
%%% @doc In-memory Merkle Patricia trees.
%%%
%%% This module is a wrapper for 'utils_mp_trees', implementing the
%%% following enhancements:
%%% * API compatible with the OTP 'gb_trees' module;
%%% * It enables better type specifications in code using this module.
%%%
%%% @see gb_trees
%%% @end
%%%-------------------------------------------------------------------
-module(utils_mtrees).

%% API - subset of OTP `gb_trees` module
-export([empty/0,
         delete/2,
         get/2,
         insert/3,
         iterator/1,
         iterator/2,
         iterator_from/2,
         iterator_from/3,
         iterator_next/1,
         lookup/2,
         enter/3,
         to_list/1]).

%% API - Merkle tree
-export([root_hash/1,
         lookup_with_proof/2,
         lookup_with_proof/3,
         verify_proof/4,
         commit_to_db/1,
         empty_with_backend/1
        ]).

%% For internal functional db
-export([ proof_db_commit/2
        , proof_db_get/2
        , proof_db_put/3
        , proof_db_fold/3
        ]).

-export_type([iterator/0,
              iterator_opts/0,
              mtree/0,
              mtree/2,
              root_hash/0,
              proof/0]).

-define(HASH_BYTES, 32).
-define(IS_KEY(K), is_binary(K)).
-define(IS_VALUE(V), is_binary(V)).

-type key() :: binary().
-type value() :: binary().
-type mtree() :: mtree(key(), value()).

-opaque iterator() :: utils_mp_trees:iterator().
-type iterator_opts() :: utils_mp_trees:iterator_opts().

%% Enable specification of types of key and value for enabling code
%% using this module to document types for readability.
%% Both key and value must be binaries.
-type mtree(_K, _V) :: utils_mp_trees:tree().

%% 256 bits as of ?HASH_BYTES * 8
-type root_hash() :: <<_:256>>.

-type proof() :: utils_mp_trees_db:db().

%%%===================================================================
%%% API - subset of OTP `gb_trees` module
%%%===================================================================

-spec empty() -> mtree().
empty() ->
    utils_mp_trees:new().

-spec empty_with_backend(utils_mp_trees_db:db()) -> mtree().
empty_with_backend(DB) ->
    utils_mp_trees:new(DB).

delete(Key, Tree) when ?IS_KEY(Key) ->
    utils_mp_trees:delete(Key, Tree).

get(Key, Tree) when ?IS_KEY(Key) ->
    case utils_mp_trees:get(Key, Tree) of
        <<>> -> error({not_present, Key});
        Val -> Val
    end.

lookup(Key, Tree) when ?IS_KEY(Key) ->
    case utils_mp_trees:get(Key, Tree) of
        <<>> ->
            none;
        Value when ?IS_VALUE(Value) ->
            {value, Value}
    end.

enter(Key, Value, Tree) when ?IS_KEY(Key), ?IS_VALUE(Value) ->
    utils_mp_trees:put(Key, Value, Tree).

insert(Key, Value, Tree) when ?IS_KEY(Key), ?IS_VALUE(Value) ->
    case lookup(Key, Tree) of
        none -> utils_mp_trees:put(Key, Value, Tree);
        {value, _} -> error({already_present, Key})
    end.

-spec iterator(mtree()) -> iterator().
iterator(Tree) ->
    utils_mp_trees:iterator(Tree).

-spec iterator(mtree(), iterator_opts()) -> iterator().
iterator(Tree, Opts) ->
    utils_mp_trees:iterator(Tree, Opts).

-spec iterator_from(key(), mtree()) -> iterator().
iterator_from(Key, Tree) ->
    utils_mp_trees:iterator_from(Key, Tree).

-spec iterator_from(key(), mtree(), iterator_opts()) -> iterator().
iterator_from(Key, Tree, Opts) ->
    utils_mp_trees:iterator_from(Key, Tree, Opts).

-spec iterator_next(iterator()) ->
                           {key(), value(), iterator()} | '$end_of_table'.
iterator_next(Iter) ->
    utils_mp_trees:iterator_next(Iter).

-spec to_list(mtree()) -> [{key(), value()}].
to_list(Tree) ->
    Iterator = utils_mp_trees:iterator(Tree),
    to_list(utils_mp_trees:iterator_next(Iterator), []).

to_list('$end_of_table', Acc) -> Acc;
to_list({Key, Val, Iter}, Acc) ->
    to_list(utils_mp_trees:iterator_next(Iter), [{Key, Val}|Acc]).

%%%===================================================================
%%% API - Merkle tree
%%%===================================================================

%% Return root hash of specified non-empty Merkle tree.
-spec root_hash(mtree()) -> {ok, root_hash()} | {error, empty}.
root_hash(Tree) ->
    case utils_mp_trees:root_hash(Tree) of
        <<>> ->
            {error, empty};
        Hash = <<_:?HASH_BYTES/unit:8>> ->
            {ok, Hash}
    end.

-spec lookup_with_proof(key(), mtree()) -> none |
                                           {value_and_proof, value(), proof()}.

%% Will use the built in dict proof db.
lookup_with_proof(Key, Tree) when ?IS_KEY(Key) ->
    lookup_with_proof(Key, Tree, new_proof_db()).

-spec lookup_with_proof(key(), mtree(), proof()) -> none |
                                                    {value_and_proof, value(), proof()}.
lookup_with_proof(Key, Tree, ProofDB) when ?IS_KEY(Key) ->
    case lookup(Key, Tree) of
        none ->
            none;
        {value, Value} ->
            {Value, Proof} = utils_mp_trees:construct_proof(Key, ProofDB, Tree),
            {value_and_proof, Value, Proof}
    end.

-spec verify_proof(key(), value(), root_hash(), proof()) -> {ok, verified} |
                                                            {error, term()}.
verify_proof(Key, Value, RootHash, Proof) ->
    case utils_mp_trees:verify_proof(Key, Value, RootHash, Proof) of
        ok -> {ok, verified};
        Other -> {error, Other}
    end.

-spec commit_to_db(mtree()) -> mtree().
commit_to_db(Tree) ->
    case utils_mp_trees:commit_to_db(Tree) of
        {ok, Tree1}   -> Tree1;
        {error, What} -> error({failed_commit, What})
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% Note that this is only the default proof db, if the caller did not
%% provide a proof db in the call.

new_proof_db() ->
    utils_mp_trees_db:new(proof_db_spec()).

proof_db_spec() ->
    #{ handle => dict:new()
     , cache  => dict:new()
     , get    => {?MODULE, proof_db_get}
     , put    => {?MODULE, proof_db_put}
     , commit => {?MODULE, proof_db_commit}
     }.

proof_db_get(Key, Proof) ->
    {value, dict:fetch(Key, Proof)}.

proof_db_put(Key, Val, Proof) ->
    dict:store(Key, Val, Proof).

proof_db_commit(_Cache,_DB) ->
    error(no_commits_in_proof).

proof_db_fold(Fun, Initial, Proof) ->
    dict:fold(Fun, Initial, Proof).
