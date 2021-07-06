# plutus-reference
A repository with some information I may need to refer later about the plutus lectures

Note: This is still being written and, because of this, a lot of information is still missing and some of it may even be false, so make sure to do your own research when needed and **do not rely on this repository for handling critical data**.

* [Ledger](#ledger)
    * [Ada](#ada)
    * [Address](#address)
    * [Api](#api)
    * [Bytes](#bytes)
    * [Contexts](#contexts)
    * [Credential](#credential)
    * [Crypto](#crypto)
    * [DCert](#dcert)
    * [Examples](#examples)
    * [Interval](#interval)
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

### [Api](https://github.com/input-output-hk/plutus/blob/master/plutus-ledger-api/src/Plutus/V1/Ledger/Api.hs)

Nothing here, feel free to contribute!

### [Bytes](https://github.com/input-output-hk/plutus/blob/master/plutus-ledger-api/src/Plutus/V1/Ledger/Bytes.hs)

Nothing here, feel free to contribute!


### [Contexts](https://github.com/input-output-hk/plutus/blob/master/plutus-ledger-api/src/Plutus/V1/Ledger/Contexts.hs)

#### TxInInfo

> An input of a pending transaction.

```haskell
data TxInInfo = TxInInfo
    { txInInfoOutRef   :: TxOutRef
    , txInInfoResolved :: TxOut
    } deriving (Generic)
```

#### ScriptPurpose

> Purpose of the script that is currently running

```haskell
data ScriptPurpose
    = Minting CurrencySymbol
    | Spending TxOutRef
    | Rewarding StakingCredential
    | Certifying DCert
```

The code definition says it all, it's a data type that defines the purpose of the current script. If a script purpose is `Miniting`, then it will need to store the `CurrencySymbol`, which is the hash of the contract that contains the minting policy.

If, alternativily, the script purpose is `Spending`, then it should store the [`TxOutRef`](#txoutref), that is: the reference to the ouput of the transaction that will be "spending".

If, in the other hand, it's purpouse is `Rewarding`, then it should store the `StakingCredential`, which is a credential containing the necessary information to assign rewards. The last one, I have no idea what is used for.

#### TxInfo

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

`txInfoInputs` is a list of inputs ([`TxInInfo`](#txininfo)), which are basically previous UTxOs (or previous outputs to be more technically correct) that were spent to generate new ones.
`txInfoOutputs` is a list of newly created outputs ([`TxOut`](#txout)).

#### ScriptContext

```haskell
data ScriptContext = ScriptContext{scriptContextTxInfo :: TxInfo, scriptContextPurpose :: ScriptPurpose }
```

`ScriptContext` stores the all the information about a possible transaction, as well as, what it is used for. Both of this actions are handled by `TxInfo` and `ScriptPurpose`, so it is simply a combination of both of this data types.

#### findOwnInput

> Find the input currently being validated.

```haskell
findOwnInput :: ScriptContext -> Maybe TxInInfo
findOwnInput ScriptContext{scriptContextTxInfo=TxInfo{txInfoInputs}, scriptContextPurpose=Spending txOutRef} =
    find (\TxInInfo{txInInfoOutRef} -> txInInfoOutRef == txOutRef) txInfoInputs
findOwnInput _ = Nothing
```

The first thing `findOwnInput` does is to limit the number of valid paramaters (only contexts with a spending purpose, for example, will pass). By doing that we get two important variables: `txInfoInputs` and `txOutRef`, the former is a list containing all inputs in the context of this script, the latter is a reference to the user's previous UTxO output (our current input). What the function does is to go over each input, extract it's `TxInInfoOutRef` and, if it is equal to our `txOutRef`, return it, otherwise return `Nothing`.

#### findDatum

> Find the data corresponding to a data hash, if there is one

```haskell
findDatum :: DatumHash -> TxInfo -> Maybe Datum
findDatum dsh TxInfo{txInfoData} = snd <$> find f txInfoData
    where
        f (dsh', _) = dsh' == dsh
```

Given a `DatumHash` and a `TxInfo`, search for a `Datum` with the corresponding hash inside `txInfoData` (an element of `TxInfo`). This is important because `TxOut`s don't store the datum it self, but it's hash, so in order to extract the data it self from a `TxOut` we need to use this function.

#### findDatumHash

> Find the hash of a datum, if it is part of the pending transaction's hashes

```haskell
findDatumHash :: Datum -> TxInfo -> Maybe DatumHash
findDatumHash ds TxInfo{txInfoData} = fst <$> find f txInfoData
    where
        f (_, ds') = ds' == ds
```

Does the same thing as `findDatum`, but instead of searching for a `Datum` given a `DatumHash`, searchs for a `DatumHash` given a `Datum`.

#### findTxInByTxOutRef

```haskell
findTxInByTxOutRef :: TxOutRef -> TxInfo -> Maybe TxInInfo
findTxInByTxOutRef outRef TxInfo{txInfoInputs} =
    find (\TxInInfo{txInInfoOutRef} -> txInInfoOutRef == outRef) txInfoInputs
```

Given a reference to an output (`TxOutRef`) and information about a transaction (`TxInfo`), `findTxInByTxOutRef` searches for an input whose reference to it's previous UTxO output is the same as the given output reference.

#### findContinuingOutputs

> Finds all the outputs that pay to the same script address that we are currently spending from, if any.

```haskell
findContinuingOutputs :: ScriptContext -> [Integer]
findContinuingOutputs ctx | Just TxInInfo{txInInfoResolved=TxOut{txOutAddress}} <- findOwnInput ctx = findIndices (f txOutAddress) (txInfoOutputs $ scriptContextTxInfo ctx)
    where
        f addr TxOut{txOutAddress=otherAddress} = addr == otherAddress
findContinuingOutputs _ = Builtins.error()
```

#### getContinuingOutputs

```haskell
getContinuingOutputs :: ScriptContext -> [TxOut]
getContinuingOutputs ctx | Just TxInInfo{txInInfoResolved=TxOut{txOutAddress}} <- findOwnInput ctx = filter (f txOutAddress) (txInfoOutputs $ scriptContextTxInfo ctx)
    where
        f addr TxOut{txOutAddress=otherAddress} = addr == otherAddress
getContinuingOutputs _ = Builtins.error()
```

#### scriptCurrencySymbol

> The 'CurrencySymbol' of a 'MintingPolicy'

```haskell
scriptCurrencySymbol :: MintingPolicy -> CurrencySymbol
scriptCurrencySymbol scrpt = let (MintingPolicyHash hsh) = mintingPolicyHash scrpt in Value.currencySymbol hsh
```

A `scriptCurrencySymbol` is the hash of the script that validates the minting policy. So that's what this function does, it gets a `MintingPolicy` script, hashes it and returns it in a `Value.currencySymbol` form.

#### txSignedBy

> Check if a transaction was signed by the given public key.

```haskell
txSignedBy :: TxInfo -> PubKeyHash -> Bool
txSignedBy TxInfo{txInfoSignatories} k = case find ((==) k) txInfoSignatories of
    Just _  -> True
    Nothing -> False
```

Goes over each element of the `txInfoSignatories` list (a component of the `TxInfo` we received) and verifies if the signature is equal to the public key hash we want to compare (`k`).


#### pubKeyOutput

> Get the public key hash that locks the transaction output, if any.

```haskell
pubKeyOutput :: TxOut -> Maybe PubKeyHash
pubKeyOutput TxOut{txOutAddress} = toPubKeyHash txOutAddress
```

Every `TxOut` contains an address and every `Address` has a Credential that can be of type `PubKeyCredential` or `ScriptCredential`. If the received output has an address with a public key credential, the function returns it's hash, otherwise it returns `Nothing`.

#### ownHashes

> Get the validator and datum hashes of the output that is curently being validated

```haskell
ownHashes :: ScriptContext -> (ValidatorHash, DatumHash)
ownHashes (findOwnInput -> Just TxInInfo{txInInfoResolved=TxOut{txOutAddress=Address (ScriptCredential s) _, txOutDatumHash=Just dh}}) = (s,dh)
ownHashes _                                                                                                                            = Builtins.error ()
```

#### ownHash

> Get the hash of the validator script that is currently being validated.

```haskell
ownHash :: ScriptContext -> ValidatorHash
ownHash p = fst (ownHashes p)
```

#### fromSymbol

> Convert a 'CurrencySymbol' to a 'ValidatorHash'

```haskell
fromSymbol :: CurrencySymbol -> ValidatorHash
fromSymbol (CurrencySymbol s) = ValidatorHash s
```

#### scriptOutputsAt

> Get the list of 'TxOut' outputs of the pending transaction at a given script address.

```haskell
scriptOutputsAt :: ValidatorHash -> TxInfo -> [(DatumHash, Value)]
scriptOutputsAt h p =
    let flt TxOut{txOutDatumHash=Just ds, txOutAddress=Address (ScriptCredential s) _, txOutValue} | s == h = Just (ds, txOutValue)
        flt _ = Nothing
    in mapMaybe flt (txInfoOutputs p)
```

Return in a `(DatumHash,Value)` format every pending transactions output that has a matching `ValidatorHash` compared to the one received (`h`). In other words, returns a list with every transaction inside a script that has this user's public key as it's output.

#### valueLockedBy

> Get the total value locked by the given validator in this transaction.

```haskell
valueLockedBy :: TxInfo -> ValidatorHash -> Value
valueLockedBy ptx h =
    let outputs = map snd (scriptOutputsAt h ptx)
    in mconcat outputs
```

#### pubKeyOutputsAt

> Get the values paid to a public key address by a pending transaction.

```haskell
pubKeyOutputsAt :: PubKeyHash -> TxInfo -> [Value]
pubKeyOutputsAt pk p =
    let flt TxOut{txOutAddress = Address (PubKeyCredential pk') _, txOutValue} | pk == pk' = Just txOutValue
        flt _                             = Nothing
    in mapMaybe flt (txInfoOutputs p)
```

#### valuePaidTo

> Get the total value paid to a public key address by a pending transaction.

```haskell
valuePaidTo :: TxInfo -> PubKeyHash -> Value
valuePaidTo ptx pkh = mconcat (pubKeyOutputsAt pkh ptx)
```

#### adaLockedBy

> Get the total amount of 'Ada' locked by the given validator in this transaction.

```haskell
adaLockedBy :: TxInfo -> ValidatorHash -> Ada
adaLockedBy ptx h = Ada.fromValue (valueLockedBy ptx h)
```

#### signsTransaction

> Check if the provided signature is the result of signing the pending transaction (without witnesses) with the given public key.

```haskell
signsTransaction :: Signature -> PubKey -> TxInfo -> Bool
signsTransaction (Signature sig) (PubKey (LedgerBytes pk)) TxInfo{txInfoId=TxId h} =
    verifySignature pk h sig
```

`signsTransaction` makes sure that the given `PubKey` actually produced `Signature` by signing `TxInfo`.

More abstractly you could think of the signature as an actual written signature, the transaction information as an important document (maybe a contract) and the public key as the person who claims to have signed this document. So you are basically checking if this person actually signed the document.

More technically, every user has a private and public key. A cryptographic signature is a function that takes a private key and an arbitrary content and returns a value (the signature). Because of some mathematical correlation between the private and public key, we can use the public key together with the signature to know if they actually match (the owner of this public key actually signed it).

#### valueSpent

> Get the total value of inputs spent by this transaction.

```haskell
valueSpent :: TxInfo -> Value
valueSpent = foldMap (txOutValue . txInInfoResolved) . txInfoInputs
```

`valueSpent` goes over each input and, not caring about it's sender, add it together.

#### valueProduced

> Get the total value of outputs produced by this transaction.

```haskell
valueProduced :: TxInfo -> Value
valueProduced = foldMap txOutValue . txInfoOutputs
```

`valueProduced` goes over each output and, not caring about it's sender, add it together. In other words, does the same thing as `valueSpent`, but with outputs.

#### ownCurrencySymbol

> The 'CurrencySymbol' of the current validator script.

```haskell
ownCurrencySymbol :: ScriptContext -> CurrencySymbol
ownCurrencySymbol ScriptContext{scriptContextPurpose=Minting cs} = cs
ownCurrencySymbol _                                              = Builtins.error ()
```

Given a `ScriptContext`, if it's purpose is `Minting`, returns the currency symbol, otherwise throws an error, as other script purposes don't have any currency sybol.

#### spendsOutput

> Check if the pending transaction spends a specific transaction output (identified by the hash of a transaction and an index into that transactions' outputs)

```haskell
spendsOutput :: TxInfo -> TxId -> Integer -> Bool
spendsOutput p h i =
    let spendsOutRef inp =
            let outRef = txInInfoOutRef inp
            in h == txOutRefId outRef
                && i == txOutRefIdx outRef

    in any spendsOutRef (txInfoInputs p)
```

### [Credential](https://github.com/input-output-hk/plutus/blob/master/plutus-ledger-api/src/Plutus/V1/Ledger/Credential.hs)

#### StakingCredential

> Staking credential used to assign rewards

```haskell
data StakingCredential
    = StakingHash Builtins.ByteString
    | StakingPtr Integer Integer Integer -- NB: The fields should really be Word64 / Natural / Natural, but 'Integer' is our only integral type so we need to use it instead.
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (ToJSON, FromJSON, Serialise, Hashable, NFData)
```

#### Credential

> Credential required to unlock a transaction output

```haskell
data Credential
  = PubKeyCredential PubKeyHash -- ^ The transaction that spends this output must be signed by the private key
  | ScriptCredential ValidatorHash -- ^ The transaction that spends this output must include the validator script and be accepted by the validator.
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (ToJSON, FromJSON, Serialise, Hashable, NFData)
```

`Credential` is used to, whenever you want to consume an UTxO, prove that you actually "own" this output.

### [Crypto](https://github.com/input-output-hk/plutus/blob/master/plutus-ledger-api/src/Plutus/V1/Ledger/Crypto.hs)

#### PubKey

> A cryptographic public key.

```haskell
newtype PubKey = PubKey { getPubKey :: LedgerBytes }
    deriving stock (Eq, Ord, Generic)
    deriving anyclass (Newtype, ToJSON, FromJSON, NFData)
    deriving newtype (P.Eq, P.Ord, Serialise, PlutusTx.IsData)
    deriving IsString via LedgerBytes
    deriving (Show, Pretty) via LedgerBytes
```

`PubKey` is someone's identity. It's used to identify the user that signed a transaction.

#### PubKeyHash

> The hash of a public key. This is frequently used to identify the public key, rather than the key itself.

```haskell
newtype PubKeyHash = PubKeyHash { getPubKeyHash :: BS.ByteString }
    deriving stock (Eq, Ord, Generic)
    deriving anyclass (ToJSON, FromJSON, Newtype, ToJSONKey, FromJSONKey, NFData)
    deriving newtype (P.Eq, P.Ord, Serialise, PlutusTx.IsData, Hashable)
    deriving IsString via LedgerBytes
    deriving (Show, Pretty) via LedgerBytes
```

It's one layer above the actual `PubKey`, usually used to identify someone without actually providing the public key it self.

#### PrivateKey

> A cryptographic private key.

```haskell
newtype PrivateKey = PrivateKey { getPrivateKey :: LedgerBytes }
    deriving stock (Eq, Ord, Generic)
    deriving anyclass (ToJSON, FromJSON, Newtype, ToJSONKey, FromJSONKey)
    deriving newtype (P.Eq, P.Ord, Serialise, PlutusTx.IsData)
    deriving (Show, Pretty) via LedgerBytes
```

The use of a `PrivateKey` is the only way to sign transactions. Only the actual user is supposed to have access to this key and, by providing the signature, the transaction content and the public key, any user can make sure that the person that signed this transaction had access to the private key and, therefore, is trustworthy.

#### Signature

> A message with a cryptographic signature.

```haskell
newtype Signature = Signature { getSignature :: Builtins.ByteString }
    deriving stock (Eq, Ord, Generic)
    deriving newtype (P.Eq, P.Ord, Serialise, PlutusTx.IsData, NFData)
    deriving (Show, Pretty) via LedgerBytes
```

A signature is a value produced by a function that takes a private key and an arbitrary content (usually a transaction) and returns a byte string that can be verified by only using the public key.

You could think it as an actual signature where the private key is the persons knowledge on how to write the symbols and letters in the correct way, while the content (or transaction) is a document and the public key is a set of signatures from this person used to verify it's validity.


#### signedBy

> Check whether the given 'Signature' was signed by the private key corresponding to the given public key.

```haskell
signedBy :: Signature -> PubKey -> TxId -> Bool
signedBy (Signature s) (PubKey k) txId =
    let k' = ED25519.publicKey $ KB.getLedgerBytes k
        s' = ED25519.signature s
    in throwCryptoError $ ED25519.verify <$> k' <*> pure (getTxId txId) <*> s' -- TODO: is this what we want
```

#### signTx

> Sign the hash of a transaction using a private key.

```haskell
signTx :: TxId -> PrivateKey -> Signature
signTx (TxId txId) = sign txId
```

#### sign

> Sign a message using a private key.

```haskell
sign :: BA.ByteArrayAccess a => a -> PrivateKey -> Signature
sign  msg (PrivateKey privKey) =
    let k  = ED25519.secretKey $ KB.getLedgerBytes privKey
        pk = ED25519.toPublic <$> k
        salt :: BS.ByteString
        salt = "" -- TODO: do we need better salt?
        convert = Signature . BS.pack . BA.unpack
    in throwCryptoError $ fmap convert (ED25519.sign <$> k <*> pure salt <*> pk <*> pure msg)
```

#### fromHex

```haskell
fromHex :: BS.ByteString -> Either String PrivateKey
fromHex = fmap PrivateKey . KB.fromHex
```

#### toPubKey

```haskell
toPublicKey :: PrivateKey -> PubKey
toPublicKey = PubKey . KB.fromBytes . BS.pack . BA.unpack . ED25519.toPublic . f . KB.bytes . getPrivateKey where
    f = throwCryptoError . ED25519.secretKey
```

#### knownPrivateKeys

```haskell
knownPrivateKeys :: [PrivateKey]
knownPrivateKeys = [privateKey1, privateKey2, privateKey3, privateKey4, privateKey5, privateKey6, privateKey7, privateKey8, privateKey9, privateKey10]
```

Arbitrary private keys usually used for testing purposes.

### [DCert](https://github.com/input-output-hk/plutus/blob/master/plutus-ledger-api/src/Plutus/V1/Ledger/DCert.hs)

#### DCert

> A representation of the ledger DCert. Some information is digested, and not included

```haskell
data DCert
  = DCertDelegRegKey StakingCredential
  | DCertDelegDeRegKey StakingCredential
  | DCertDelegDelegate
      StakingCredential
      -- ^ delegator
      PubKeyHash
      -- ^ delegatee
  | -- | A digest of the PoolParams
    DCertPoolRegister
      PubKeyHash
      -- ^ poolId
      PubKeyHash
      -- ^ pool VFR
  | -- | The retiremant certificate and the Epoch N
    DCertPoolRetire PubKeyHash Integer -- NB: Should be Word64 but we only have Integer on-chain
  | -- | A really terse Digest
    DCertGenesis
  | -- | Another really terse Digest
    DCertMir
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (ToJSON, FromJSON, Serialise, Hashable, NFData)
```

I don't know what this is, feel free to make a pull request if you do.

### [Examples](https://github.com/input-output-hk/plutus/blob/master/plutus-ledger-api/src/Plutus/V1/Ledger/Examples.hs)

#### alwaysSucceedingNAryFunction

> Creates a script which has N arguments, and always succeeds.

```haskell
alwaysSucceedingNAryFunction :: Natural -> SerializedScript
alwaysSucceedingNAryFunction n = toShort $ toStrict $ serialise $ Scripts.Script $ UPLC.Program () (PLC.defaultVersion ()) (body n)
    where
        -- No more arguments! The body can be anything that doesn't fail, so we return `\x . x`
        body i | i == 0 = UPLC.LamAbs() (UPLC.DeBruijn 0) $ UPLC.Var () (UPLC.DeBruijn 1)
        -- We're using de Bruijn indices, so we can use the same binder each time!
        body i = UPLC.LamAbs () (UPLC.DeBruijn 0) $ body (i-1)
```

#### alwaysFailingNAryFunction

> Creates a script which has N arguments, and always fails.

```haskell
alwaysFailingNAryFunction :: Natural -> SerializedScript
alwaysFailingNAryFunction n = toShort $ toStrict $ serialise $ Scripts.Script $ UPLC.Program () (PLC.defaultVersion ()) (body n)
    where
        -- No more arguments! The body should be error.
        body i | i == 0 = UPLC.Error ()
        -- We're using de Bruijn indices, so we can use the same binder each time!
        body i = UPLC.LamAbs () (UPLC.DeBruijn 0) $ body (i-1)
```

### [Interval](https://github.com/input-output-hk/plutus/blob/master/plutus-ledger-api/src/Plutus/V1/Ledger/Interval.hs)

#### Interval

> An interval of @a@s.
>
> The interval may be either closed or open at either end, meaning
> that the endpoints may or may not be included in the interval.
>
> The interval can also be unbounded on either side.

```haskell
data Interval a = Interval { ivFrom :: LowerBound a, ivTo :: UpperBound a }
    deriving stock (Haskell.Eq, Haskell.Ord, Haskell.Show, Generic)
    deriving anyclass (FromJSON, ToJSON, Serialise, Hashable, NFData)
```

Because Cardano is a decentralised system and sometimes it may take a while for a transaction to spread, it's very useful to pass deadlines or simmilar arguments in intervals of time instead of periods. `Interval` is a data type that is used to do exactly that.

#### Extended

> A set extended with a positive and negative infinity.

```haskell
data Extended a = NegInf | Finite a | PosInf
    deriving stock (Haskell.Eq, Haskell.Ord, Haskell.Show, Generic)
    deriving anyclass (FromJSON, ToJSON, Serialise, Hashable, NFData)
```

Extends a value to include infinity

#### Closure

> Whether a bound is inclusive or not.

```haskell
type Closure = Bool
```

Given an interval 0 to 10, we can include 0 or exclude it and the same thing goes to 10. For instance if we define ]0, 10] (excluding 0), 0.0001 would be inside the interval, but not 0 it self.

#### UpperBound

> The upper bound of an interval.

```haskell
data UpperBound a = UpperBound (Extended a) Closure
    deriving stock (Haskell.Eq, Haskell.Ord, Haskell.Show, Generic)
    deriving anyclass (FromJSON, ToJSON, Serialise, Hashable, NFData)
```

A data type for deining upper bounds, it takes an extended value and a closure. The value indicates the bound it self (`Finite 7` or `PosInf` for instance) and the closure indicates if the value is included in the interval.

#### LowerBound

> The lower bound of an interval.

```haskell
data LowerBound a = LowerBound (Extended a) Closure
    deriving stock (Haskell.Eq, Haskell.Ord, Haskell.Show, Generic)
    deriving anyclass (FromJSON, ToJSON, Serialise, Hashable, NFData)
```

A data type for deining lower bounds

#### strictUpperBound

```haskell
strictUpperBound :: a -> UpperBound a
strictUpperBound a = UpperBound (Finite a) False
```

An `UpperBound` that doesn't include it's value, only what's smaller

#### strictLowerBound

```haskell
strictLowerBound :: a -> LowerBound a
strictLowerBound a = LowerBound (Finite a) False
```

A `LowerBound` that doesn't include it's value, only what's greater


#### lowerBound

```haskell
lowerBound :: a -> LowerBound a
lowerBound a = LowerBound (Finite a) True
```

A `LowerBound` that does include it's value together with greater values

#### upperBound

```haskell
upperBound :: a -> UpperBound a
upperBound a = UpperBound (Finite a) True
```

An `UpperBound` that does include it's value together with smaller values

#### interval

> @interval a b@ includes all values that are greater than or equal to @a@ and smaller than or equal to @b@. Therefore it includes @a@ and @b@.

```haskell
interval :: a -> a -> Interval a
interval s s' = Interval (lowerBound s) (upperBound s')
```

Defines an interval from a given lower bound to a given upper bound, including the values themself

#### singleton

```haskell
singleton :: a -> Interval a
singleton s = interval s s
```

Makes an interval representation of a point in time. Because a `singleton` only cares about one value, the upper bound is equal to the lower bound

#### from

> @from a@ is an 'Interval' that includes all values that are greater than or equal to @a@.

```haskell
from :: a -> Interval a
from s = Interval (lowerBound s) (UpperBound PosInf True)
```

Defines an interval that goes from the given value to infinity. This means that, given a value A, any other value greater or equal to A is part of this interval

#### to

> @to a@ is an 'Interval' that includes all values that are smaller than @a@.

```haskell
to :: a -> Interval a
to s = Interval (LowerBound NegInf True) (upperBound s)
```

Defines an interval that goes from the given value to negative infinity. This means that, given a value A, any other value less or equal to A is part of this interval

#### always

> An 'Interval' that covers every slot.

```haskell
always :: Interval a
always = Interval (LowerBound NegInf True) (UpperBound PosInf True)
```


#### never

> An 'Interval' that is empty.

```haskell
never :: Interval a
never = Interval (LowerBound PosInf True) (UpperBound NegInf True)
```

#### member

> Check whether a value is in an interval.

```haskell
member :: Ord a => a -> Interval a -> Bool
member a i = i `contains` singleton a
```

#### overlaps

> Check whether two intervals overlap, that is, whether there is a value that is a member of both intervals.

```haskell
overlaps :: Ord a => Interval a -> Interval a -> Bool
overlaps l r = not $ isEmpty (l `intersection` r)
```

#### intersection

> 'intersection a b' is the largest interval that is contained in 'a' and in 'b', if it exists.

```haskell
intersection :: Ord a => Interval a -> Interval a -> Interval a
intersection (Interval l1 h1) (Interval l2 h2) = Interval (max l1 l2) (min h1 h2)
```

#### hull

> 'hull a b' is the smallest interval containing 'a' and 'b'.

```haskell
hull :: Ord a => Interval a -> Interval a -> Interval a
hull (Interval l1 h1) (Interval l2 h2) = Interval (min l1 l2) (max h1 h2)
```

#### contains

> @a `contains` b@ is true if the 'Interval' @b@ is entirely contained in @a@. That is, @a `contains` b@ if for every entry @s@, if @member s b@ then @member s a@.

```haskell
contains :: Ord a => Interval a -> Interval a -> Bool
contains (Interval l1 h1) (Interval l2 h2) = l1 <= l2 && h2 <= h1
```

#### isEmpty

> Check if an 'Interval' is empty.

```haskell
isEmpty :: Ord a => Interval a -> Bool
isEmpty (Interval (LowerBound v1 in1) (UpperBound v2 in2)) = case v1 `compare` v2 of
    LT -> False
    GT -> True
    EQ -> not (in1 && in2)
```

#### before

> Check if a value is earlier than the beginning of an 'Interval'.

```haskell
before :: Ord a => a -> Interval a -> Bool
before h (Interval f _) = lowerBound h < f
```

#### after

> Check if a value is later than the end of a 'Interval'.

```haskell
after :: Ord a => a -> Interval a -> Bool
after h (Interval _ t) = upperBound h > t
```




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
