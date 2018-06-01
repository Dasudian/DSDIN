

-type(option()  :: {atom(), any()}).
-type(options() :: [option()]).


-record(worker_info, {tag   :: atom(),
                      mon   :: reference(),
                      timer :: {t, timer:tref()} | 'no_timer'}).

-type worker_info() :: #worker_info{}.
-type workers() :: orddict:orddict(pid(), worker_info()).
-type mining_state() :: 'running' | 'stopped'.

-record(candidate, {block     :: dsdc_blocks:block(),
                    bin       :: binary(), %% Serialized for hash
                    nonce     :: dsdc_pow:nonce(),
                    max_nonce :: dsdc_pow:nonce(),
                    top_hash  :: binary()
                   }).

-record(state, {block_candidate                   :: #candidate{} | 'undefined',
                new_candidate_available = false   :: boolean(),
                blocked_tags            = []      :: ordsets:ordset(atom()),
                keys_ready              = false   :: boolean(),
                mining_state            = running :: mining_state(),
                seen_top_block_hash               :: binary() | 'undefined',
                workers                 = []      :: workers()
               }).
