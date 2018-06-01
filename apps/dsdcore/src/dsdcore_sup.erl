%%% -*- erlang-indent-level:4; indent-tabs-mode: nil -*-
%%%=============================================================================
%%% @copyright 2018, Dasudian Technologies
%%% @doc
%%%    Supervisor for the core application
%%%
%%%  Full supervision tree is
%%%```
%%%       dsdcore_sup  (one_for_one)
%%%           |
%%%           --------------------------------------------------
%%%           |         |         |           |       |        |
%%%           |   dsdc_metrics  dsdc_keys  dsdc_tx_pool  |  dsdc_subscribe
%%%           |                                       |
%%%   dsdc_connection_sup  (one_for_all)         dsdc_conductor_sup (rest_for_one)
%%%           |                                       |
%%%           |                                       ---------------------
%%%           |                                       |                   |
%%%           |                                 dsdc_block_generator  dsdc_conductor
%%%           |
%%%           ---------------------------------------------------
%%%           |                     |          |                |
%%%   dsdc_peer_connection_sup   dsdc_peers   dsdc_sync  dsdc_connection_listener
%%%     (simple_one_for_one)
%%%           |
%%%           --------------------
%%%           |             |
%%%   dsdc_peer_connection  ...
%%%'''
%%%
%%% @end
%%%=============================================================================
-module(dsdcore_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-include("blocks.hrl").

-define(SERVER, ?MODULE).
-define(CHILD(Mod,N,Type), {Mod,{Mod,start_link,[]},permanent,N,Type,[Mod]}).
-define(CHILD(Mod,N,Type,Params), {Mod,{Mod,start_link,Params},permanent,N,Type,[Mod]}).


%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

init([]) ->
    {ok, {{one_for_one, 5, 10}, [watchdog_childspec(),
                                 ?CHILD(dsdc_metrics_rpt_dest, 5000, worker),
                                 ?CHILD(dsdc_keys, 5000, worker),
                                 ?CHILD(dsdc_tx_pool, 5000, worker),
                                 ?CHILD(dsdc_conductor_sup, 5000, supervisor),
                                 ?CHILD(dsdc_subscribe, 5000, worker),
                                 ?CHILD(dsdc_connection_sup, 5000, supervisor)
                                ]
         }}.


watchdog_childspec() ->
    {watchdog, {gen_serv, start, [watchdog]},
     permanent, 5000, worker, [watchdog]}.
