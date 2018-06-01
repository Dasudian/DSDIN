-define(ttl_delta_int, 0).
-define(ttl_delta_atom, delta).
-define(ttl_block_int, 1).
-define(ttl_block_atom, block).

-record(oracle_register_tx, {
          account                                     :: dsdc_keys:pubkey(),
          nonce                                       :: integer(),
          query_spec    = <<"string()">>              :: dsdo_oracles:type_spec(),
          response_spec = <<"boolean() | integer()">> :: dsdo_oracles:type_spec(),
          query_fee                                   :: integer(),
          oracle_ttl                                  :: dsdo_oracles:ttl(),
          fee                                         :: integer(),
          ttl                                         :: dsdc_blocks:height()
          }).

-record(oracle_extend_tx, {
          oracle     :: dsdc_keys:pubkey(),
          nonce      :: integer(),
          oracle_ttl :: dsdo_oracles:relative_ttl(),
          fee        :: integer(),
          ttl        :: dsdc_blocks:height()
          }).

-record(oracle_query_tx, {
          sender       :: dsdc_keys:pubkey(),
          nonce        :: integer(),
          oracle       :: dsdc_keys:pubkey(),
          query        :: dsdo_oracles:query(),
          query_fee    :: integer(),
          query_ttl    :: dsdo_oracles:ttl(),
          response_ttl :: dsdo_oracles:relative_ttl(),
          fee          :: integer(),
          ttl          :: dsdc_blocks:height()
          }).

-record(oracle_response_tx, {
          oracle   :: dsdc_keys:pubkey(),
          nonce    :: integer(),
          query_id :: dsdo_query:id(),
          response :: dsdo_oracles:response(),
          fee      :: integer(),
          ttl      :: dsdc_blocks:height()
          }).
