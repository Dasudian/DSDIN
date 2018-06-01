
-module(dsdct_call_state_tree).

%% API
-export([ commit_to_db/1
        , empty/0
        , empty_with_backend/0
        , get_call/3
        , insert_call/2
        , lookup_call/3
        , prune/2
        , root_hash/1]).

-export_type([tree/0]).

%%%===================================================================
%%% Types
%%%===================================================================

-type call_tree() :: dsdu_mtrees:mtree().

-record(call_tree, {
          calls = dsdu_mtrees:empty() :: call_tree()
    }).

-opaque tree() :: #call_tree{}.

%%%===================================================================
%%% API
%%%===================================================================

-spec empty() -> tree().
empty() ->
    #call_tree{}.

-spec empty_with_backend() -> tree().
empty_with_backend() ->
    CtTree = dsdu_mtrees:empty_with_backend(dsdc_db_backends:calls_backend()),
    #call_tree{calls = CtTree}.

%% A new block always starts with an empty calls tree.
%% Calls and return values are only keept for one block.
-spec prune(dsdc_blocks:height(), dsdc_trees:trees()) -> dsdc_trees:trees().
prune(_,Trees) ->
    dsdc_trees:set_calls(Trees, empty_with_backend()).

-spec insert_call(dsdct_call:call(), tree()) -> tree().
insert_call(Call, Tree = #call_tree{ calls = CtTree}) ->
    %% Construct the Id to store in the tree.
    CtId       = dsdct_call:contract_address(Call),
    CallId     = dsdct_call:id(Call),
    CallTreeId = call_tree_id(CtId, CallId),

    %% Insert the new call into the history
    Serialized = dsdct_call:serialize(Call),
    CtTree1    = dsdu_mtrees:insert(CallTreeId, Serialized, CtTree),

    %% Update the calls tree
    Tree#call_tree{ calls = CtTree1}.

-spec lookup_call(dsdct_contracts:id(), dsdct_call:id(), tree()) -> {value, dsdct_call:call()} | none.
lookup_call(CtId, CallId, Tree) ->
    case dsdu_mtrees:lookup(call_tree_id(CtId, CallId), Tree#call_tree.calls) of
        {value, Val} -> {value, dsdct_call:deserialize(Val)};
        none         -> none
    end.

-spec get_call(dsdct_contracts:id(), dsdct_call:id(), tree()) -> dsdct_call:call().
get_call(CtId, CallId, #call_tree{ calls = CtTree }) ->
    CallTreeId = call_tree_id(CtId, CallId),
    dsdct_call:deserialize(dsdu_mtrees:get(CallTreeId, CtTree)).

%% -- Hashing --

-spec root_hash(tree()) -> {ok, dsdu_mtrees:root_hash()} | {error, empty}.
root_hash(#call_tree{calls = CtTree}) ->
    dsdu_mtrees:root_hash(CtTree).

%% -- Commit to db --

-spec commit_to_db(tree()) -> tree().
commit_to_db(#call_tree{calls = CtTree} = Tree) ->
    Tree#call_tree{calls = dsdu_mtrees:commit_to_db(CtTree)}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

call_tree_id(ContractId, CallId) ->
    <<ContractId/binary, CallId/binary>>.
