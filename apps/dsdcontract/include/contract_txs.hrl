
-record(contract_create_tx, {
          owner      :: dsdc_keys:pubkey(),
          nonce      :: non_neg_integer(),
          code       :: binary(),
          vm_version :: dsdct_contracts:vm_version(),
          fee        :: dsdct_contracts:amount(),
          deposit    :: dsdct_contracts:amount(),
          amount     :: dsdct_contracts:amount(),
          gas        :: dsdct_contracts:amount(),
          gas_price  :: dsdct_contracts:amount(),
          call_data  :: binary(),
          ttl        :: dsdc_blocks:height()
        }).

-record(contract_call_tx, {
          caller     :: dsdc_keys:pubkey(),
          nonce      :: integer(),
          contract   :: dsdc_keys:pubkey(),
          vm_version :: dsdct_contracts:vm_version(),
          fee        :: integer(),
          ttl        :: dsdc_blocks:height(),
          amount     :: dsdct_contracts:amount(),
          gas        :: dsdct_contracts:amount(),
          gas_price  :: dsdct_contracts:amount(),
          call_data  :: binary(),
          call_stack = [] :: [non_neg_integer()]
            %% addresses (the pubkey as an integer) of contracts on the call
            %% stack
          }).

