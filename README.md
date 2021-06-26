# plutus-reference
A repository with some information I may need to refer to later about the plutus lectures

## [Tx](https://github.com/input-output-hk/plutus/blob/master/plutus-ledger-api/src/Plutus/V1/Ledger/Tx.hs#LC128)

> A transaction, including witnesses for its inputs.

```haskell
data Tx = Tx {
    txInputs      :: Set.Set TxIn,
    -- ^ The inputs to this transaction.
    txCollateral  :: Set.Set TxIn,
    -- ^ The collateral inputs to cover the fees in case validation of the transaction fails.
    txOutputs     :: [TxOut],
    -- ^ The outputs of this transaction, ordered so they can be referenced by index.
    txMint        :: !Value,
    -- ^ The 'Value' minted by this transaction.
    txFee         :: !Value,
    -- ^ The fee for this transaction.
    txValidRange  :: !SlotRange,
    -- ^ The 'SlotRange' during which this transaction may be validated.
    txMintScripts :: Set.Set MintingPolicy,
    -- ^ The scripts that must be run to check minting conditions.
    txSignatures  :: Map PubKey Signature,
    -- ^ Signatures of this transaction.
    txRedeemers   :: Redeemers,
    -- ^ Redeemers of the minting scripts.
    txData        :: Map DatumHash Datum
    -- ^ Datum objects recorded on this transaction.
    } deriving stock (Show, Eq, Generic)
      deriving anyclass (ToJSON, FromJSON, Serialise, NFData)
```

`Tx` is the transaction it self. The most important paramaters are `txInputs` and `txOutputs`, which are responsible for actually changing the users wallet, as well as `txSignatures`, that proves the transaction was approved by the affected users, and `txData`, which stores whatever may be important to know about this transaction.

## [TxOut](https://github.com/input-output-hk/plutus/blob/master/plutus-ledger-api/src/Plutus/V1/Ledger/Tx.hs#LC384)

> A transaction output, consisting of a target address, a value, and optionally a datum hash

```haskell
data TxOut = TxOut {
    txOutAddress   :: Address,
    txOutValue     :: Value,
    txOutDatumHash :: Maybe DatumHash
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (Serialise, ToJSON, FromJSON, NFData)
```

It is, as the name suggests, a represantation of a transaction output.


## [TxOutRef](https://github.com/input-output-hk/plutus/blob/master/plutus-ledger-api/src/Plutus/V1/Ledger/Tx.hs#LC301)


> A reference to a transaction output. This is a pair of a transaction reference, and an index indicating which of the outputs of that transaction we are referring to.

```haskell
data TxOutRef = TxOutRef {
    txOutRefId  :: TxId,
    txOutRefIdx :: Integer -- ^ Index into the referenced transaction's outputs
    }
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (Serialise, ToJSON, FromJSON, ToJSONKey, FromJSONKey, NFData)
```
It's basically a reference to a transaction output. In that sense, it takes a reference to the transaction it self (it's ID) and an the output index, but it doesn't store the transaction it self, but rather a reference to it.

## [TxOutTx](https://github.com/input-output-hk/plutus/blob/master/plutus-ledger-api/src/Plutus/V1/Ledger/Tx.hs#LC431)

> A 'TxOut' along with the 'Tx' it comes from, which may have additional information e.g. the full data script that goes with the 'TxOut'.

```haskell
data TxOutTx = TxOutTx { txOutTxTx :: Tx, txOutTxOut :: TxOut }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (Serialise, ToJSON, FromJSON)
```

Very simmilarly to `TxOutRef`, `TxOutTx` also joins two existing data types (`TxOut` and `Tx`), but, differently, doesn't store a pointer to the transaction, but the transaction it self, as it may be helpfull to not only know where a transaction output came from, but also explore it's paramaters.

## [TxIn](https://github.com/input-output-hk/plutus/blob/master/plutus-ledger-api/src/Plutus/V1/Ledger/Tx.hs#LC331)

> A transaction input, consisting of a transaction output reference and an input type.

```haskell
data TxIn = TxIn {
    txInRef  :: !TxOutRef,
    txInType :: Maybe TxInType
    }
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (Serialise, ToJSON, FromJSON, NFData)
```

Very simmilarly to `TxOut`, `TxIn` is simply a represantation of a transaction input.
