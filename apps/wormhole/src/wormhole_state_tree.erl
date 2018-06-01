
-module(wormhole_state_tree).

%% API
-export([commit_to_db/1,
         delete/2,
         empty/0,
         empty_with_backend/0,
         enter/2,
         get/2,
         lookup/2,
         root_hash/1]).

%%%===================================================================
%%% Types
%%%===================================================================

-type channel() :: wormhole_channels:channel().

-type chkey() :: wormhole_channels:id().
-type chvalue() :: wormhole_channels:serialized().

-opaque tree() :: aeu_mtrees:mtree(chkey(), chvalue()).

-export_type([tree/0]).

%%%===================================================================
%%% API
%%%===================================================================

-spec commit_to_db(tree()) -> tree().
commit_to_db(Tree) ->
    aeu_mtrees:commit_to_db(Tree).

-spec delete(wormhole_channels:id(), tree()) -> tree().
delete(Id, Tree) ->
    aeu_mtrees:delete(Id, Tree).

-spec empty() -> tree().
empty() ->
    aeu_mtrees:empty().

-spec empty_with_backend() -> tree().
empty_with_backend() ->
    aeu_mtrees:empty_with_backend(aec_db_backends:channels_backend()).

-spec enter(channel(), tree()) -> tree().
enter(Channel, Tree) ->
    Id         = wormhole_channels:id(Channel),
    Serialized = wormhole_channels:serialize(Channel),
    aeu_mtrees:enter(Id, Serialized, Tree).

-spec get(wormhole_channels:id(), tree()) -> wormhole_channels:channel().
get(Id, Tree) ->
    wormhole_channels:deserialize(aeu_mtrees:get(Id, Tree)).

-spec lookup(wormhole_channels:id(), tree()) -> {value, channel()} | none.
lookup(Id, Tree) ->
    case aeu_mtrees:lookup(Id, Tree) of
        {value, Val} -> {value, wormhole_channels:deserialize(Val)};
        none         -> none
    end.

-spec root_hash(tree()) -> {ok, aeu_mtrees:root_hash()} | {error, empty}.
root_hash(Tree) ->
    aeu_mtrees:root_hash(Tree).
