
-module(dsdc_spend_tx).

%% API
-export([new/1,
         type/0,
         fee/1,
         ttl/1,
         nonce/1,
         origin/1,
         recipient/1,
         check/5,
         process/5,
         accounts/1,
         signers/2,
         serialization_template/1,
         serialize/1,
         deserialize/2,
         for_client/1
        ]).

-export([payload/1]).

-behavior(dsdtx).

-include("blocks.hrl").

-define(SPEND_TX_VSN, 1).
-define(SPEND_TX_TYPE, spend_tx).

-record(spend_tx, {
          sender    = <<>>          :: dsdc_keys:pubkey(),
          recipient = <<>>          :: dsdc_keys:pubkey(),
          amount    = 0             :: non_neg_integer(),
          fee       = 0             :: non_neg_integer(),
          ttl       = 0             :: dsdc_blocks:height(),
          nonce     = 0             :: non_neg_integer(),
          payload   = <<>>          :: binary()}).

-opaque tx() :: #spend_tx{}.

-export_type([tx/0]).

-spec new(map()) -> {ok, dsdtx:tx()}.
new(#{sender := SenderPubkey,
      recipient := RecipientPubkey,
      amount := Amount,
      fee := Fee,
      ttl := TTL,
      nonce := Nonce,
      payload := Payload}) when is_integer(Amount), Amount >= 0,
                                is_integer(Nonce), Nonce >= 0,
                                is_integer(Fee), Fee >= 0,
                                is_integer(TTL), TTL >= 0,
                                is_binary(SenderPubkey),
                                is_binary(RecipientPubkey),
                                is_binary(Payload)
                                ->
    Tx = #spend_tx{sender = SenderPubkey,
                   recipient = RecipientPubkey,
                   amount = Amount,
                   fee = Fee,
                   ttl = TTL,
                   nonce = Nonce,
                   payload = Payload},
    {ok, dsdtx:new(?MODULE, Tx)}.

-spec type() -> atom().
type() ->
    ?SPEND_TX_TYPE.

-spec fee(tx()) -> integer().
fee(#spend_tx{fee = F}) ->
    F.

-spec ttl(tx()) -> dsdc_blocks:height().
ttl(#spend_tx{ttl = TTL}) ->
    TTL.

-spec nonce(tx()) -> non_neg_integer().
nonce(#spend_tx{nonce = Nonce}) ->
    Nonce.

-spec origin(tx()) -> dsdc_keys:pubkey().
origin(#spend_tx{sender = Sender}) ->
    Sender.

-spec recipient(tx()) -> dsdc_keys:pubkey().
recipient(#spend_tx{recipient = Recipient}) ->
    Recipient.

-spec payload(tx()) -> binary().
payload(#spend_tx{payload = Payload}) ->
    Payload.

-spec check(tx(), dsdtx:tx_context(), dsdc_trees:trees(), dsdc_blocks:height(), non_neg_integer()) ->
        {ok, dsdc_trees:trees()} | {error, term()}.
check(#spend_tx{} = SpendTx, _Context, Trees, Height, _ConsensusVersion) ->
    RecipientPubkey = recipient(SpendTx),
    Checks = [fun check_sender_account/3],
    case dsdu_validation:run(Checks, [SpendTx, Trees, Height]) of
        ok ->
            {ok, dsdc_trees:ensure_account(RecipientPubkey, Trees)};
        {error, _Reason} = Error ->
            Error
    end.

-spec accounts(tx()) -> [dsdc_keys:pubkey()].
accounts(#spend_tx{sender = SenderPubKey, recipient = RecipientPubKey}) ->
    [SenderPubKey, RecipientPubKey].

-spec signers(tx(), dsdc_trees:trees()) -> {ok, [dsdc_keys:pubkey()]}.
signers(#spend_tx{sender = SenderPubKey}, _) -> {ok, [SenderPubKey]}.

-spec process(tx(), dsdtx:tx_context(), dsdc_trees:trees(), dsdc_blocks:height(), non_neg_integer()) -> {ok, dsdc_trees:trees()}.
process(#spend_tx{sender = SenderPubkey,
                  recipient = RecipientPubkey,
                  amount = Amount,
                  fee = Fee,
                  nonce = Nonce}, _Context, Trees0, _Height, _ConsensusVersion) ->
    AccountsTrees0 = dsdc_trees:accounts(Trees0),

    {value, SenderAccount0} = dsdc_accounts_trees:lookup(SenderPubkey, AccountsTrees0),
    {ok, SenderAccount} = dsdc_accounts:spend(SenderAccount0, Amount + Fee, Nonce),
    AccountsTrees1 = dsdc_accounts_trees:enter(SenderAccount, AccountsTrees0),

    {value, RecipientAccount0} = dsdc_accounts_trees:lookup(RecipientPubkey, AccountsTrees1),
    {ok, RecipientAccount} = dsdc_accounts:earn(RecipientAccount0, Amount),
    AccountsTrees2 = dsdc_accounts_trees:enter(RecipientAccount, AccountsTrees1),

    Trees = dsdc_trees:set_accounts(Trees0, AccountsTrees2),
    {ok, Trees}.

serialize(#spend_tx{sender = Sender,
                    recipient = Recipient,
                    amount = Amount,
                    fee = Fee,
                    ttl = TTL,
                    nonce = Nonce,
                    payload = Payload}) ->
    {version(),
     [ {sender, Sender}
     , {recipient, Recipient}
     , {amount, Amount}
     , {fee, Fee}
     , {ttl, TTL}
     , {nonce, Nonce}
     , {payload, Payload}
     ]}.

deserialize(?SPEND_TX_VSN,
            [ {sender, Sender}
            , {recipient, Recipient}
            , {amount, Amount}
            , {fee, Fee}
            , {ttl, TTL}
            , {nonce, Nonce}
            , {payload, Payload}]) ->
    #spend_tx{sender = Sender,
              recipient = Recipient,
              amount = Amount,
              fee = Fee,
              ttl = TTL,
              nonce = Nonce,
              payload = Payload}.

serialization_template(?SPEND_TX_VSN) ->
    [ {sender, binary}
    , {recipient, binary}
    , {amount, int}
    , {fee, int}
    , {ttl, int}
    , {nonce, int}
    , {payload, binary}
    ].

for_client(#spend_tx{sender = Sender,
                     recipient = Recipient,
                     amount = Amount,
                     fee = Fee,
                     ttl = TTL,
                     nonce = Nonce,
                     payload = Payload}) ->
    #{<<"sender">> => dsdc_base58c:encode(account_pubkey, Sender),
      <<"data_schema">> => <<"SpendTxJSON">>, % swagger schema name
      <<"recipient">> => dsdc_base58c:encode(account_pubkey, Recipient),
      <<"amount">> => Amount,
      <<"fee">> => Fee,
      <<"ttl">> => TTL,
      <<"nonce">> => Nonce,
      <<"payload">> => Payload,
      <<"vsn">> => version()}.

%% Internals

-spec check_sender_account(tx(), dsdc_trees:trees(), dsdc_blocks:height()) ->
                                  ok | {error, term()}.
check_sender_account(#spend_tx{sender = SenderPubkey, amount = Amount,
                               fee = Fee, nonce = TxNonce }, Trees, _Height) ->
    dsdtx_utils:check_account(SenderPubkey, Trees, TxNonce, Fee + Amount).

version() ->
    ?SPEND_TX_VSN.
