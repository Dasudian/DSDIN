
-module(dsdc_poi).

-export([ add_poi/3
        , new/1
        , root_hash/1
        , verify/3
        ]).

-export([ from_serialization_format/1
        , serialization_format/1
        , serialization_format_template/0
        ]).

-export([ proof_db_get/2
        , proof_db_put/3
        , proof_db_commit/2
        ]).

-export_type([ poi/0
             ]).

-include("blocks.hrl").

-define(HASH_BYTES, 32).

-record(dsdc_poi, { proof     :: dsdu_mp_trees_db:db()
                 , root_hash :: binary()
                 }).

-opaque poi()     :: #dsdc_poi{}.

-type key() :: binary().
-type value() :: binary().
-type proof_value() :: dsdu_rlp:encodable().

%%%===================================================================
%%% API
%%%===================================================================

-spec new(state_hash()) -> poi().
new(<<_:?HASH_BYTES/binary>> = Hash) ->
    #dsdc_poi{ proof     = new_proof_db()
            , root_hash = Hash
            }.

-spec root_hash(poi()) -> state_hash().
root_hash(#dsdc_poi{root_hash = Hash}) ->
    Hash.

-spec add_poi(key(), dsdu_mtrees:mtree(), poi()) ->
                     {'ok', value(), poi()}
                   | {'error', 'not_present' | 'wrong_root_hash'}.
add_poi(Key, Tree, #dsdc_poi{root_hash = RootHash} = Poi) ->
    case dsdu_mtrees:root_hash(Tree) of
        {ok, RootHash} ->
            Proof = Poi#dsdc_poi.proof,
            case dsdu_mtrees:lookup_with_proof(Key, Tree, Proof) of
                none -> {error, not_present};
                {value_and_proof, Value, NewProof} ->
                    { ok
                    , Value
                    , Poi#dsdc_poi{ proof = NewProof}
                    }
            end;
        Other ->
            lager:debug("Root hash: ~p\nOther: ~p\n", [RootHash, Other]),
            {error, wrong_root_hash}
    end.

-spec verify(key(), term(), poi()) -> 'ok' | {'error', term()}.
verify(Key, Value, #dsdc_poi{root_hash = RootHash} = Poi) ->
    case dsdu_mtrees:verify_proof(Key, Value, RootHash, Poi#dsdc_poi.proof) of
        {ok, verified} -> ok;
        {error, _} = E -> E
    end.

-spec serialization_format(poi()) -> {state_hash(), [{key(), proof_value()}]}.
serialization_format(#dsdc_poi{ root_hash = Hash
                             , proof = ProofDb}) ->
    { Hash
    , proof_serialize_to_list(ProofDb)
    }.

-spec serialization_format_template() -> {'binary', [{'binary', ['binary']}]}.
serialization_format_template() ->
    { binary
    , [{binary, [binary]}]
    }.

-spec from_serialization_format({state_hash(),  [{key(), proof_value()}]}) ->
                                       poi().
from_serialization_format({<<_:?HASH_BYTES/binary>> = Hash, ProofList}) ->
    #dsdc_poi{ root_hash = Hash
            , proof = proof_from_list(ProofList)
            }.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%%===================================================================
%%% Proof

new_proof_db() ->
    dsdu_mp_trees_db:new(proof_db_spec()).

proof_db_spec() ->
    #{ handle => gb_trees
     , cache  => gb_trees:empty()
     , get    => {?MODULE, proof_db_get}
     , put    => {?MODULE, proof_db_put}
     , commit => {?MODULE, proof_db_commit}
     }.

proof_db_get(Key, Proof) ->
    gb_trees:lookup(Key, Proof).

proof_db_put(Key, Val, Proof) ->
    gb_trees:enter(Key, Val, Proof).

proof_db_commit(_Cache,_DB) ->
    error(no_commits_in_proof).

proof_serialize_to_list(Proof) ->
    gb_trees:to_list(dsdu_mp_trees_db:get_cache(Proof)).

proof_from_list(ProofList) ->
    case build_proof_list(ProofList, new_proof_db()) of
        {ok, Proof} -> Proof;
        error -> error(illegal_proof)
    end.

build_proof_list([{Key, Val}|Left], Proof) when is_binary(Key),
                                                is_list(Val) ->
    case lists:all(fun is_binary/1, Val) of
        true -> build_proof_list(Left, dsdu_mp_trees_db:put(Key, Val, Proof));
        false -> error
    end;
build_proof_list([], Proof) ->
    {ok, Proof};
build_proof_list(_, _) ->
    error.



