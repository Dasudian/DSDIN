-module(dsdva_compile_tests).

-include_lib("eunit/include/eunit.hrl").

compile_test() ->
    {ok,Cwd} = file:get_cwd(),
    Dir = "apps/dsdvarna/test/contracts",
    {ok,Fs} = file:list_dir(Dir),
    Cfun = fun (F) ->
		   case string:split(F, ".") of
		       [_,"dsdv"] ->
			   Name = Cwd ++ "/" ++ Dir ++ "/" ++ F,
			   ?assertMatch({ok,_,_}, dsdva_compile:file(Name));
		       _ -> ok
		   end
	   end,
    lists:foreach(Cfun, Fs).
