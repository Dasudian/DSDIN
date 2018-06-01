
-module(dsdso_icode).

-export([pp/1]).

-include("dsdso_icode.hrl").

pp(Icode) ->
    %% TODO: Actually do *Pretty* printing.
    io:format("~p~n", [Icode]).
