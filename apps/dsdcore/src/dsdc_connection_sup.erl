%%% -*- erlang-indent-level:4; indent-tabs-mode: nil -*-
%%%=============================================================================
%%% @copyright 2018, Dasudian Technologies
%%% @doc
%%%    Supervisor for servers dealing with inter node communication.
%%%
%%%    Individual connections cannot bring down the central servers (dsdc_peers,
%%%    dsdc_sync), but if one of those goes down, all connections are brought
%%%    down, and the whole peer/sync handling is restarted.
%%%
%%%    It is the responsibility of dsdc_peers to handle that a connection
%%%    (dsdc_peer_connections) goes down. And the strategy there is to
%%%    re-establish connections where the node is the initiator, and delegate
%%%    the corresponding responsibility to the remote node.
%%%
%%% @end
%%%=============================================================================
-module(dsdc_connection_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

init([]) ->
    %% We want all children brought down and restarted
    SupFlags = #{ strategy => one_for_all
                , intensity => 5
                , period => 10},
    ChildSpecs = [ dsdc_peer_connection_sup_spec() % outgoing connections
                 , worker(dsdc_peers)
                 , worker(dsdc_sync)
                 , peer_listener_spec()           % incoming connections
                 ],
    {ok, {SupFlags, ChildSpecs}}.

worker(Mod) ->
    child(Mod, worker, []).

peer_listener_spec() ->
    NumAcceptors = acceptors(),
    MaxConnections = max_connections(),
    {ok, SecKey} = dsdc_keys:peer_privkey(),
    {ok, PubKey} = dsdc_keys:peer_pubkey(),
    ranch:child_spec(dsdc_peer, NumAcceptors,
                     ranch_tcp, [
                         {port, sync_port()},
                         {ip, sync_listen_address()},
                         {max_connections, MaxConnections}
                     ],
                     dsdc_peer_connection, #{
                         ext_sync_port => ext_sync_port(),
                         seckey => SecKey,
                         pubkey => PubKey
                     }
                    ).

dsdc_peer_connection_sup_spec() ->
    child(dsdc_peer_connection_sup, supervisor, [ext_sync_port()]).

child(Mod, Type, Args) ->
    #{ id => Mod
     , start => {Mod, start_link, Args}
     , restart => permanent
     , shutdown => 5000
     , type => Type
     }.

%%====================================================================
%% Shared configs
%%====================================================================

-define(DEFAULT_SYNC_PORT, 3015).
-define(DEFAULT_SYNC_LISTEN_ADDRESS, <<"0.0.0.0">>).
-define(DEFAULT_ACCEPTORS, 10).
-define(DEFAULT_MAX_CONNECTIONS, 100).

sync_port() ->
    dsdu_env:user_config_or_env([<<"sync">>, <<"port">>], dsdcore, sync_port, ?DEFAULT_SYNC_PORT).

ext_sync_port() ->
    dsdu_env:user_config_or_env([<<"sync">>, <<"external_port">>],
        dsdcore, ext_sync_port, sync_port()).

sync_listen_address() ->
    Config = dsdu_env:user_config_or_env([<<"sync">>, <<"listen_address">>],
                dsdcore, sync_listen_address, ?DEFAULT_SYNC_LISTEN_ADDRESS),
    {ok, IpAddress} = inet:parse_address(binary_to_list(Config)),
    IpAddress.

acceptors() ->
    dsdu_env:user_config_or_env([<<"sync">>, <<"acceptors">>],
                               dsdcore, sync_acceptors,
                               ?DEFAULT_ACCEPTORS).

max_connections() ->
    dsdu_env:user_config_or_env([<<"sync">>, <<"max_connections">>],
                               dsdcore, sync_max_connections,
                               ?DEFAULT_MAX_CONNECTIONS).
