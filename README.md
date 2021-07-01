# plutus-reference
A repository with some information I may need to refer later about the plutus lectures

* [Ledger](#ledger)
    * [Ada](#ada)
    * [Address](#address)
    * [Api]()
    * [Bytes]()
    * [Contexts]()
    * [Credential]()
    * [Crypto]()
    * [DCert]()
    * [Examples]()
    * [Interval]()
    * [Orphans]()
    * [Scripts]()
    * [Slot]()
    * [Time]()
    * [Tx]()
    * [TxId]()
    * [Value]()

## [Ledger](https://github.com/input-output-hk/plutus/blob/master/plutus-ledger-api/src/Plutus/V1/Ledger)

### [Ada](https://github.com/input-output-hk/plutus/blob/master/plutus-ledger-api/src/Plutus/V1/Ledger/Ada.hs)

#### adaSymbol

> The 'CurrencySymbol' of the 'Ada' currency.

```haskell
adaSymbol :: CurrencySymbol
adaSymbol = TH.currencySymbol emptyByteString
```

A `CurrencySymbol` is the hash of a script that will be executed to validate minting and burning. Because ADA is deflationary, it uses an "empty script", which means no one is able to mint nor burn any ADA. That's why `adaSymbol` is an `emptyByteString`.

#### adaToken

> The 'TokenName' of the 'Ada' currency.

```haskell
adaToken :: TokenName
adaToken = TH.tokenName emptyByteString
```

A `TokenName` is a string that represents a token (pretty self explanatory). So, for instance, if Bitcoin was a Plutus token, it's token name would probably be `"btc"`. In this spirit, `adaToken` is simply Ada's token name, but because Ada is the Cardano's "default token", it doesn't receive an `"ada"` token name, but instead an `emptyByteString`.

#### Ada

> ADA, the special currency on the Cardano blockchain. The unit of Ada is Lovelace, and 1M Lovelace is one Ada. See note [Currencies] in 'Ledger.Validation.Value.TH'.

```haskell
newtype Ada = Lovelace { getLovelace :: Integer }
    deriving (Haskell.Enum)
    deriving stock (Haskell.Eq, Haskell.Ord, Haskell.Show, Generic)
    deriving anyclass (ToJSON, FromJSON)
    deriving newtype (Eq, Ord, Haskell.Num, AdditiveSemigroup, AdditiveMonoid, AdditiveGroup, MultiplicativeSemigroup, MultiplicativeMonoid, Haskell.Integral, Haskell.Real, Serialise, PlutusTx.IsData)
    deriving Pretty via (Tagged "Lovelace:" Integer)

instance Haskell.Semigroup Ada where
    Lovelace a1 <> Lovelace a2 = Lovelace (a1 + a2)

instance Semigroup Ada where
    Lovelace a1 <> Lovelace a2 = Lovelace (a1 + a2)

instance Haskell.Monoid Ada where
    mempty = Lovelace 0

instance Monoid Ada where
    mempty = Lovelace 0

makeLift ''Ada
```

#### getAda

> Get the amount of Ada (the unit of the currency Ada) in this 'Ada' value.

```haskell
getAda :: Ada -> Micro
getAda (Lovelace i) = MkFixed i
```

`Micro` is a representation of µ, which is just another way of saying `10^-6`. So, what `getAda` is doing is getting a `Lovelace` amount and wrapping it in a type that is a million times smaller than the value it holds.

#### toValue

> Create a 'Value' containing only the given 'Ada'.

```haskell
toValue :: Ada -> Value
toValue (Lovelace i) = TH.singleton adaSymbol adaToken i
```

Based on an Ada (an amount basically), create a `Value`, which contains the token symbol, name and amount (in Ada).

#### fromValue

> Get the 'Ada' in the given 'Value'.

```haskell
fromValue :: Value -> Ada
fromValue v = Lovelace (TH.valueOf v adaSymbol adaToken)
```

Ignores everything except the amount from a `Value`. So, for instance, one might want to verify if someone has sufficient funds. For that, he might not need to know the token's name or symbol, so he should use `fromValue` to extract the Ada amount only.

#### lovelaceOf

> Create 'Ada' representing the given quantity of Lovelace (the unit of the currency Ada).

```haskell
lovelaceOf :: Integer -> Ada
lovelaceOf = Lovelace
```

#### adaOf

> Create 'Ada' representing the given quantity of Ada (1M Lovelace).

```haskell
adaOf :: Micro -> Ada
adaOf (MkFixed x) = Lovelace x
```

#### lovelaceValueOf

> A 'Value' with the given amount of Lovelace (the currency unit).
>
> @lovelaceValueOf == toValue . lovelaceOf@

```haskell
lovelaceValueOf :: Integer -> Value
lovelaceValueOf = TH.singleton adaSymbol adaToken
```

#### adaValueOf

> A 'Value' with the given amount of Ada (the currency unit).
>
> @adaValueOf == toValue . adaOf@

```haskell
adaValueOf :: Micro -> Value
adaValueOf (MkFixed x) = TH.singleton adaSymbol adaToken x
```

#### divide

> Divide one 'Ada' value by another.

```haskell
divide :: Ada -> Ada -> Ada
divide (Lovelace a) (Lovelace b) = Lovelace (P.divide a b)
```


#### isZero

> Check whether an 'Ada' value is zero.

```haskell
isZero :: Ada -> Bool
isZero (Lovelace i) = i == 0
```

### [Address](https://github.com/input-output-hk/plutus/blob/master/plutus-ledger-api/src/Plutus/V1/Ledger/Address.hs)

#### Address

> Address with two kinds of credentials, normal and staking

```haskell
data Address = Address{ addressCredential :: Credential, addressStakingCredential :: (Maybe StakingCredential)}
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (ToJSON, FromJSON, ToJSONKey, FromJSONKey, Serialise, Hashable, NFData)

instance Pretty Address where
    pretty (Address cred stakingCred) =
        let staking = maybe "no staking credential" pretty stakingCred in
        "addressed to" <+> pretty cred <+> parens staking


instance PlutusTx.Eq Address where
    Address cred stakingCred == Address cred' stakingCred' =
        cred PlutusTx.== cred'
        PlutusTx.&& stakingCred PlutusTx.== stakingCred'
```

`Address` is a data type that holds a `Credential` and a `StakingCredential`, The `Credential` is a data type that either holds a user's public key hash or a script validator hash. The `StakingCredential`, which can be `Nothing`, holds either a `StakingHash` or a `StakingPtr` and is used to assign rewards. 

To be honest, I don't quite understand what a staking credential actually is, so fell free to submit a Pull Request giving a better explanation.

#### pubKeyAddress

> The address that should be targeted by a transaction output locked by the given public key.

```haskell
pubKeyAddress :: PubKey -> Address
pubKeyAddress pk = Address (PubKeyCredential (pubKeyHash pk)) Nothing
```

Takes the user's `PubKey`, hash it, makes a credential out of it and gives it as an `Address` constructor input.

#### pubKeyHashAddress

> The address that should be targeted by a transaction output locked by the public key with the given hash.

```haskell
pubKeyHashAddress :: PubKeyHash -> Address
pubKeyHashAddress pkh = Address (PubKeyCredential pkh) Nothing
```

The only difference from `pubKeyAddress` is that it already receives a hashed public key so it doesn't need to hash it again.


#### toPubKeyHash

> The PubKeyHash of the address, if any

```haskell
toPubKeyHash :: Address -> Maybe PubKeyHash
toPubKeyHash (Address (PubKeyCredential k) _) = Just k
toPubKeyHash _                                = Nothing
```

Takes an `Address`, verify if it's credential is a public key one and, if so, returns it. If, in the other hand, is a script validator, returns `Nothing`.

#### toValidatorHash

> The validator hash of the address, if any

```haskell
toValidatorHash :: Address -> Maybe ValidatorHash
toValidatorHash (Address (ScriptCredential k) _) = Just k
toValidatorHash _   
```

Same thing as `toPubKeyHash`, but instead of making sure that the address credential is a public key, it want's a script validator credential.

#### scriptAddress

> The address that should be used by a transaction output locked by the given validator script.

```haskell
scriptAddress :: Validator -> Address
scriptAddress validator = Address (ScriptCredential (validatorHash validator)) Nothing 
```

Same thing as `pubKeyAddress`, but with a script validator instead of a public key. Takes the user's `Validator`, hash it, makes a credential out of it and gives it as an `Address` constructor input.

#### scriptHashAddress

> he address that should be used by a transaction output locked by the given validator script hash.

```haskell
scriptHashAddress :: ValidatorHash -> Address
scriptHashAddress vh = Address (ScriptCredential vh) Nothing
```

Takes the user's `ValidatorHash`, makes a credential out of it and gives it as an `Address` constructor input.

#### stakingCredential

> The staking credential of an address (if any)

```haskell
stakingCredential :: Address -> Maybe StakingCredential
stakingCredential (Address _ s) = s
```
Returns a `StakingCredential` if the given address has one

## [PlutusTx](https://github.com/input-output-hk/plutus/tree/master/plutus-tx/src/PlutusTx)

### IsData

> A typeclass for types that can be converted to and from 'Data'.

```haskell
class IsData (a :: Type) where
    toData :: a -> Data
    -- TODO: this should probably provide some kind of diagnostics
    fromData :: Data -> Maybe a
```

`IsData` is a class type that transforms normal types into Datum types or vice-versa. So, for instance, if we have a transaction that stores as Datum an integer and we want to use this integer and add it to another number, we should use the `fromData` method inside the `IsData` class.

## [Transactions](https://github.com/input-output-hk/plutus/blob/master/plutus-ledger-api/src/Plutus/V1/Ledger/Tx.hs)

### Tx

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

### TxOut

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

It is, as the name suggests, a represantation of a transaction output. In other words, the value received by an address at the end of an UTXO and it's corresponding datum. 


### TxOutRef


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

### TxOutTx

> A 'TxOut' along with the 'Tx' it comes from, which may have additional information e.g. the full data script that goes with the 'TxOut'.

```haskell
data TxOutTx = TxOutTx { txOutTxTx :: Tx, txOutTxOut :: TxOut }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (Serialise, ToJSON, FromJSON)
```

Stores a transaction output together with the transaction it self, as it may be helpfull to not only know where a transaction output came from, but also explore it's paramaters.

### TxIn

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

## [Context](https://github.com/input-output-hk/plutus/blob/master/plutus-ledger-api/src/Plutus/V1/Ledger/Contexts.hs)

### ScriptPurpose

> Purpose of the script that is currently running

```haskell
data ScriptPurpose
    = Minting CurrencySymbol
    | Spending TxOutRef
    | Rewarding StakingCredential
    | Certifying DCert
```

The code definition says it all, it's a data type that defines the purpose of the current script. If a script purpose is `Miniting`, then it will need to store the `CurrencySymbol`, which is the hash of the contract that contains the minting policy. If, alternativily, the script purpose is `Spending`, then it should store the [`TxOutRef`](#txoutref), that is: the reference to the ouput of the transaction that will be "spending". The last two ones are not so common.

### TxInfo

> A pending transaction. This is the view as seen by validator scripts, so some details are stripped out.

```haskell
data TxInfo = TxInfo
    { txInfoInputs      :: [TxInInfo] -- ^ Transaction inputs
    , txInfoOutputs     :: [TxOut] -- ^ Transaction outputs
    , txInfoFee         :: Value -- ^ The fee paid by this transaction.
    -- TODO: rename to txInfoMint, but this requires changes in cardano-ledger-specs
    , txInfoForge       :: Value -- ^ The 'Value' minted by this transaction.
    , txInfoDCert       :: [DCert] -- ^ Digests of certificates included in this transaction
    , txInfoWdrl        :: [(StakingCredential, Integer)] -- ^ Withdrawals
    , txInfoValidRange  :: POSIXTimeRange -- ^ The valid range for the transaction.
    , txInfoSignatories :: [PubKeyHash] -- ^ Signatures provided with the transaction, attested that they all signed the tx
    , txInfoData        :: [(DatumHash, Datum)]
    , txInfoId          :: TxId
    -- ^ Hash of the pending transaction (excluding witnesses)
    } deriving (Generic)
```

When validating a transaction inside a contract, it is important to know some information about this transaction, this is what `TxInfo` is used for. In order to make sure, for example, that a token is being minted by someone with permission, the script could verify if any `PubKeyHash` of `txInfoSignatories` is inside the list of allowed users.

### ScriptContext

```haskell
data ScriptContext = ScriptContext{scriptContextTxInfo :: TxInfo, scriptContextPurpose :: ScriptPurpose }
```

`ScriptContext` stores the all the information about a possible transaction, as well as, what it is used for. Both of this actions are handled by `TxInfo` and `ScriptPurpose`, so it is simply a combination of both of this data types.

### findDatum

> Find the data corresponding to a data hash, if there is one

```haskell
findDatum :: DatumHash -> TxInfo -> Maybe Datum
findDatum dsh TxInfo{txInfoData} = snd <$> find f txInfoData
    where
        f (dsh', _) = dsh' == dsh
```

Because `TxOut`s don't store the datum it self, but it's hash, it is important to have a helper function that, inside `TxInfo` searches for a `Datum` based on a gived `DatumHash`. That's what `findDatum` does.

## [Contract](https://github.com/input-output-hk/plutus/tree/master/plutus-contract/src/Plutus/Contract)

### TxConstraint

> Constraints on transactions that want to spend script outputs

```haskell
data TxConstraint =
    MustIncludeDatum Datum
    | MustValidateIn POSIXTimeRange
    | MustBeSignedBy PubKeyHash
    | MustSpendAtLeast Value
    | MustProduceAtLeast Value
    | MustSpendPubKeyOutput TxOutRef
    | MustSpendScriptOutput TxOutRef Redeemer
    | MustMintValue MintingPolicyHash Redeemer TokenName Integer
    | MustPayToPubKey PubKeyHash Value
    | MustPayToOtherScript ValidatorHash Datum Value
    | MustHashDatum DatumHash Datum
    deriving stock (Haskell.Show, Generic, Haskell.Eq)
    deriving anyclass (ToJSON, FromJSON)
```

When wanting to create a new transaction, it would be very boring to write it's definition from scratch. Thinking about this, Plutus has helper functions that magically create transactions from scratch. In order to add functionalities, though, this magic functions require constraints. That's what `TxConstraint` is used for. So, for instance, if I want to create a transaction that requires the signature of a trusted user, I could use this function and give a `TxConstraint` of type `MustBeSignedBy` as an argument.

### InputConstraint

```haskell
data InputConstraint a =
    InputConstraint
        { icRedeemer :: a
        , icTxOutRef :: TxOutRef
        } deriving stock (Haskell.Show, Generic, Haskell.Functor)
```

Simmilarly to [`TxConstraint`](#txconstraint), we can create `InputConstraint`s, that basically say "This input need to follow these conditions: ...".

### OutputConstraint

```haskell
data OutputConstraint a =
    OutputConstraint
        { ocDatum :: a
        , ocValue :: Value
        } deriving stock (Haskell.Show, Generic, Haskell.Functor)
```

[`InputConstraint`](#inputconstraint), but instead of being used to create inputs, is used to create outputs.

### awaitSlot

> Wait until the slot

```haskell
awaitSlot ::
    forall w s e.
    ( AsContractError e
    )
    => Slot
    -> Contract w s e Slot
awaitSlot s = pabReq (AwaitSlotReq s) E._AwaitSlotResp
```

### currentSlot

> Get the current slot number

```haskell
currentSlot ::
    forall w s e.
    ( AsContractError e
    )
    => Contract w s e Slot
currentSlot = pabReq CurrentSlotReq E._CurrentSlotResp
```

### waitNSlots

> Wait for a number of slots to pass

```haskell
waitNSlots ::
  forall w s e.
  ( AsContractError e
  )
  => Natural
  -> Contract w s e Slot
waitNSlots n = do
  c <- currentSlot
  awaitSlot $ c + fromIntegral n
```

### utxoAt

> Get the unspent transaction outputs at an address.

```haskell
utxoAt ::
    forall w s e.
    ( AsContractError e
    )
    => Address
    -> Contract w s e UtxoMap
utxoAt addr = fmap utxo $ pabReq (UtxoAtReq addr) E._UtxoAtResp
```

### ownPubKey

> Get a public key belonging to the wallet that runs this contract.
>   * Any funds paid to this public key will be treated as the wallet's own
>     funds
>   * The wallet is able to sign transactions with the private key of this
>     public key, for example, if the public key is added to the
>     'requiredSignatures' field of 'Tx'.
>   * There is a 1-n relationship between wallets and public keys (although in
>     the mockchain n=1)

```haskell
ownPubKey :: forall w s e. (AsContractError e) => Contract w s e PubKey
ownPubKey = pabReq OwnPublicKeyReq E._OwnPublicKeyResp
```

### submitTx

> Build a transaction that satisfies the constraints, then submit it to the network. The constraints do not refer to any typed script inputs or outputs.

```haskell
submitTx :: forall w s e.
  ( AsContractError e
  )
  => TxConstraints Void Void
  -> Contract w s e Tx
submitTx = submitTxConstraintsWith @Void mempty
```

### submitTxConstraints

> Build a transaction that satisfies the constraints, then submit it to the network. Using the current outputs at the contract address and the contract's own public key to solve the constraints.

```haskell
submitTxConstraints
  :: forall a w s e.
  ( PlutusTx.IsData (RedeemerType a)
  , PlutusTx.IsData (DatumType a)
  , AsContractError e
  )
  => TypedValidator a
  -> TxConstraints (RedeemerType a) (DatumType a)
  -> Contract w s e Tx
submitTxConstraints inst = submitTxConstraintsWith (Constraints.typedValidatorLookups inst)
```

### submitTxConstraintsWith

> Build a transaction that satisfies the constraints, then submit it to the network. Using the given constraints.

```haskell
submitTxConstraintsWith
  :: forall a w s e.
  ( PlutusTx.IsData (RedeemerType a)
  , PlutusTx.IsData (DatumType a)
  , AsContractError e
  )
  => ScriptLookups a
  -> TxConstraints (RedeemerType a) (DatumType a)
  -> Contract w s e Tx

submitTxConstraintsWith sl constraints = do
  tx <- either (throwError . review _ConstraintResolutionError) pure (Constraints.mkTx sl constraints)
  submitUnbalancedTx tx
```


### ownPubKey

> Get a public key belonging to the wallet that runs this contract.
   * Any funds paid to this public key will be treated as the wallet's own
     funds
   * The wallet is able to sign transactions with the private key of this
     public key, for example, if the public key is added to the
     'requiredSignatures' field of 'Tx'.
   * There is a 1-n relationship between wallets and public keys (although in
     the mockchain n=1)

```haskell
ownPubKey :: forall w s e. (AsContractError e, HasOwnPubKey s) => Contract w s e PubKey
ownPubKey = requestMaybe @w @OwnPubKeySym @_ @_ @s WaitingForPubKey Just
```
