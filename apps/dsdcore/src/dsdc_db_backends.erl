

-module(dsdc_db_backends).

-export([ accounts_backend/0
        , calls_backend/0
        , channels_backend/0
        , contracts_backend/0
        , ns_backend/0
        , ns_cache_backend/0
        , oracles_backend/0
        , oracles_cache_backend/0
        ]).

%% Callbacks for dsdu_mp_trees_db
-export([ db_commit/2
        , db_get/2
        , db_put/3
        ]).

%%%===================================================================
%%% API
%%%===================================================================

-spec accounts_backend() -> dsdu_mp_trees_db:db().
accounts_backend() ->
    dsdu_mp_trees_db:new(db_spec(accounts)).

-spec calls_backend() -> dsdu_mp_trees_db:db().
calls_backend() ->
    dsdu_mp_trees_db:new(db_spec(calls)).

-spec channels_backend() -> dsdu_mp_trees_db:db().
channels_backend() ->
    dsdu_mp_trees_db:new(db_spec(channels)).

-spec contracts_backend() -> dsdu_mp_trees_db:db().
contracts_backend() ->
    dsdu_mp_trees_db:new(db_spec(contracts)).

-spec ns_backend() -> dsdu_mp_trees_db:db().
ns_backend() ->
    dsdu_mp_trees_db:new(db_spec(ns)).

-spec ns_cache_backend() -> dsdu_mp_trees_db:db().
ns_cache_backend() ->
    dsdu_mp_trees_db:new(db_spec(ns_cache)).

-spec oracles_backend() -> dsdu_mp_trees_db:db().
oracles_backend() ->
    dsdu_mp_trees_db:new(db_spec(oracles)).

-spec oracles_cache_backend() -> dsdu_mp_trees_db:db().
oracles_cache_backend() ->
    dsdu_mp_trees_db:new(db_spec(oracles_cache)).


%%%===================================================================
%%% Internal functions
%%%===================================================================

db_spec(Type) ->
    #{ handle => Type
     , cache  => {gb_trees, gb_trees:empty()}
     , get    => {?MODULE, db_get}
     , put    => {?MODULE, db_put}
     , commit => {?MODULE, db_commit}
     }.

db_get(Key, {gb_trees, Tree}) ->
    gb_trees:lookup(Key, Tree);
db_get(Key, accounts) ->
    dsdc_db:find_accounts_node(Key);
db_get(Key, calls) ->
    dsdc_db:find_calls_node(Key);
db_get(Key, channels) ->
    dsdc_db:find_channels_node(Key);
db_get(Key, contracts) ->
    dsdc_db:find_contracts_node(Key);
db_get(Key, ns) ->
    dsdc_db:find_ns_node(Key);
db_get(Key, ns_cache) ->
    dsdc_db:find_ns_cache_node(Key);
db_get(Key, oracles) ->
    dsdc_db:find_oracles_node(Key);
db_get(Key, oracles_cache) ->
    dsdc_db:find_oracles_cache_node(Key).

db_put(Key, Val, {gb_trees, Tree}) ->
    {gb_trees, gb_trees:enter(Key, Val, Tree)};
db_put(Key, Val, accounts = Handle) ->
    ok = dsdc_db:write_accounts_node(Key, Val),
    {ok, Handle};
db_put(Key, Val, channels = Handle) ->
    ok = dsdc_db:write_channels_node(Key, Val),
    {ok, Handle};
db_put(Key, Val, ns = Handle) ->
    ok = dsdc_db:write_ns_node(Key, Val),
    {ok, Handle};
db_put(Key, Val, ns_cache = Handle) ->
    ok = dsdc_db:write_ns_cache_node(Key, Val),
    {ok, Handle};
db_put(Key, Val, calls = Handle) ->
    ok = dsdc_db:write_calls_node(Key, Val),
    {ok, Handle};
db_put(Key, Val, contracts = Handle) ->
    ok = dsdc_db:write_contracts_node(Key, Val),
    {ok, Handle};
db_put(Key, Val, oracles = Handle) ->
    ok = dsdc_db:write_oracles_node(Key, Val),
    {ok, Handle};
db_put(Key, Val, oracles_cache = Handle) ->
    ok = dsdc_db:write_oracles_cache_node(Key, Val),
    {ok, Handle}.

db_commit(Handle, {gb_trees, Cache}) ->
    Iter = gb_trees:iterator(Cache),
    db_commit_1(Handle, gb_trees:next(Iter)).

db_commit_1(Handle, none) -> {ok, Handle, {gb_trees, gb_trees:empty()}};
db_commit_1(Handle, {Key, Val, Iter}) ->
    {ok, Handle} = db_put(Key, Val, Handle),
    db_commit_1(Handle, gb_trees:next(Iter)).
