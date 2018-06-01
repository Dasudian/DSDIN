
-module(dsdo_oracles_mock).

-compile(export_all).
-compile(nowarn_export_all).

%% dsdc_tx_sign
data(S) -> S.
verify(_) -> ok.
serialize_to_binary(X) -> term_to_binary(X).

%% dsdc_target
verify(_, _) -> ok.
