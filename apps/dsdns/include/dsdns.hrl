%%%=============================================================================
%%% @copyright (C) 2018, Dasudian Technologies
%%% @doc
%%%    Records for entities that are used by dispatcher in dsdc
%%% @end
%%%=============================================================================

-record(commitment,
        {hash    :: binary(),
         owner   :: dsdc_keys:pubkey(),
         created :: dsdc_blocks:height(),
         expires :: dsdc_blocks:height()
         }).

-type name_status() :: claimed | revoked.

-record(name,
        {hash            :: binary(),
         owner           :: dsdc_keys:pubkey(),
         expires         :: dsdc_blocks:height(),
         status          :: name_status(),
         client_ttl = 0  :: integer(),
         pointers   = [] :: list()}).
