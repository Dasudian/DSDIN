-module(dsdso_eunit_SUITE).

-compile([export_all, nowarn_export_all]).

-include_lib("common_test/include/ct.hrl").

all() ->
    [{group, eunit}].

groups() ->
    [{eunit, [], [dsdso_scan_tests, dsdso_parser_tests, dsdso_compiler_tests]}].

dsdso_scan_tests(_Config)   -> ok = eunit:test(dsdso_scan_tests).
dsdso_parser_tests(_Config) -> ok = eunit:test(dsdso_parser_tests).
dsdso_compiler_tests(_Config) -> ok = eunit:test(dsdso_compiler_tests).
