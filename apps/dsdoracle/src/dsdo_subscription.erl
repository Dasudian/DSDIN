
%% API
-module(dsdo_subscription).

-export([notify_query_tx/2,
         notify_response_tx/2]).

%%%===================================================================
%%% API
%%%===================================================================

-spec notify_query_tx(dsdtx:tx(),
                      list({dsdc_subscribe:id(), dsdc_subscribe:event()})) -> ok.
notify_query_tx(Tx, Subs) ->
    {oracle_query_tx, QTx} = dsdtx:specialize_type(Tx),
    [ WS ! {event, oracle_query_tx, QTx} || {{ws, WS}, {oracle, {query, OId}}} <- Subs,
                                            OId == dsdo_query_tx:oracle(QTx) ],
    ok.

-spec notify_response_tx(dsdtx:tx(),
                         list({dsdc_subscribe:id(), dsdc_subscribe:event()})) -> ok.
notify_response_tx(Tx, Subs) ->
    {oracle_response_tx, RTx} = dsdtx:specialize_type(Tx),
    [ WS ! {event, oracle_response_tx, RTx} || {{ws, WS}, {oracle, {response, QId}}} <- Subs,
                                               QId == dsdo_response_tx:query_id(RTx) ],
    ok.

