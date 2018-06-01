
-module(dsdns_names).

-include("dsdns.hrl").

%% API
-export([id/1,
         new/3,
         update/3,
         revoke/3,
         transfer/2,
         serialize/1,
         deserialize/1]).

%% Getters
-export([owner/1,
         status/1,
         expires/1,
         pointers/1,
         client_ttl/1]).

%%%===================================================================
%%% Types
%%%===================================================================
-opaque name() :: #name{}.

-type id() :: binary().
-type serialized() :: binary().

-export_type([id/0,
              name/0,
              serialized/0]).

-define(NAME_TYPE, name).
-define(NAME_VSN, 1).

%%%===================================================================
%%% API
%%%===================================================================

-spec id(name()) -> dsdns_hash:name_hash().
id(N) ->
    hash(N).

-spec new(dsdns_claim_tx:tx(), non_neg_integer(), dsdc_blocks:height()) -> name().
new(ClaimTx, Expiration, BlockHeight) ->
    Expires    = BlockHeight + Expiration,
    Name       = dsdns_claim_tx:name(ClaimTx),
    {ok, Hash} = dsdns:get_name_hash(Name),
    %% TODO: add assertions on fields, similarily to what is done in dsdo_oracles:new/2
    #name{hash    = Hash,
          owner   = dsdns_claim_tx:account(ClaimTx),
          expires = Expires,
          status  = claimed}.

-spec update(dsdns_update_tx:tx(), name(), dsdc_blocks:height()) -> name().
update(UpdateTx, Name, BlockHeight) ->
    Expires = BlockHeight + dsdns_update_tx:name_ttl(UpdateTx),
    Name#name{expires    = Expires,
              client_ttl = dsdns_update_tx:client_ttl(UpdateTx),
              pointers   = dsdns_update_tx:pointers(UpdateTx)}.

-spec revoke(name(), non_neg_integer(), dsdc_blocks:height()) -> name().
revoke(Name, Expiration, BlockHeight) ->
    Expires = BlockHeight + Expiration,
    Name#name{status  = revoked,
              expires = Expires}.

-spec transfer(dsdns_transfer_tx:tx(), name()) -> name().
transfer(TransferTx, Name) ->
    Name#name{owner = dsdns_transfer_tx:recipient_account(TransferTx)}.

-spec serialize(name()) -> binary().
serialize(#name{} = N) ->
    dsdc_object_serialization:serialize(
      ?NAME_TYPE,
      ?NAME_VSN,
      serialization_template(?NAME_VSN),
      [ {hash, hash(N)}
      , {owner, owner(N)}
      , {expires, expires(N)}
      , {status, atom_to_binary(status(N), utf8)}
      , {client_ttl, client_ttl(N)}
      , {pointers, jsx:encode(pointers(N))}]). %% TODO: This might be ambigous

-spec deserialize(binary()) -> name().
deserialize(Bin) ->
    [ {hash, Hash}
    , {owner, Owner}
    , {expires, Expires}
    , {status, Status}
    , {client_ttl, CTTL}
    , {pointers, Pointers}
    ] = dsdc_object_serialization:deserialize(
          ?NAME_TYPE,
          ?NAME_VSN,
          serialization_template(?NAME_VSN),
          Bin),
    #name{hash       = Hash,
          owner      = Owner,
          expires    = Expires,
          status     = binary_to_existing_atom(Status, utf8),
          client_ttl = CTTL,
          pointers   = jsx:decode(Pointers)}. %% TODO: This might be ambigous

serialization_template(?NAME_VSN) ->
    [ {hash, binary}
    , {owner, binary}
    , {expires, int}
    , {status, binary}
    , {client_ttl, int}
    , {pointers, binary} %% TODO: This needs to be stricter
    ].

%%%===================================================================
%%% Getters
%%%===================================================================

-spec owner(name()) -> dsdc_keys:pubkey().
owner(N) -> N#name.owner.

-spec status(name()) -> name_status().
status(N) -> N#name.status.

-spec expires(name()) -> dsdc_blocks:height().
expires(N) -> N#name.expires.

-spec pointers(name()) -> list().
pointers(N) -> N#name.pointers.

-spec client_ttl(name()) -> integer().
client_ttl(N) -> N#name.client_ttl.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec hash(name()) -> dsdns_hash:name_hash().
hash(N) -> N#name.hash.
