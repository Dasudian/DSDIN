%% -*- mode: erlang; erlang-indent-level: 4; indent-tabs-mode: nil -*-

-record(dsdc_blocks             , {key, txs}).
-record(dsdc_headers            , {key, value, height}).
-record(dsdc_call_state         , {key, value}).
-record(dsdc_contract_state     , {key, value}).
-record(dsdc_chain_state        , {key, value}).
-record(dsdc_block_state        , {key, value, difficulty, fork_id}).
-record(dsdc_oracle_cache       , {key, value}).
-record(dsdc_oracle_state       , {key, value}).
-record(dsdc_account_state      , {key, value}).
-record(dsdc_channel_state      , {key, value}).
-record(dsdc_name_service_cache , {key, value}).
-record(dsdc_name_service_state , {key, value}).

-record(dsdc_signed_tx          , {key, value}).
-record(dsdc_tx_location        , {key, value}).
-record(dsdc_tx_pool            , {key, value}).

