module Test.Pos.Core.TestTxp
       ( tests
       ) where

import           Universum

import qualified Data.ByteString.Lazy as LB
import           Data.Maybe (fromJust)
import           Hedgehog (checkParallel, discover, forAll, Gen, property,
                           Property)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import           Pos.Binary.Core ()
import           Pos.Core.Common (Address (..), Coin (..),
                                  IsBootstrapEraAddr (..), makePubKeyAddress,
                                  Script (..))
import           Pos.Core.Txp (Tx (..), TxIn (..), TxInWitness (..),
                               TxOut (..), TxSig, TxSigData (..))
import           Pos.Crypto.Configuration
import           Pos.Crypto.Hashing (unsafeHash)
import           Pos.Crypto.Signing  (PublicKey (..),
                                      redeemDeterministicKeyGen, redeemPkBuild,
                                      RedeemPublicKey (..),
                                      RedeemSecretKey (..), redeemSign,
                                      RedeemSignature (..), SecretKey (..))
import           Pos.Crypto.Signing.Signing (createKeypairFromSeed, sign )
import           Pos.Crypto.Signing (SignTag (..))
import           Pos.Data.Attributes (mkAttributes)
import           Test.Pos.Binary.Tripping

tests :: IO Bool
tests = checkParallel $$discover

----------------------------------------------------------------------------
-- Hedgehog Generators
----------------------------------------------------------------------------

-- TODO: Double check this
-- | Generates a 'PublicKey'.
genPubKey :: Gen PublicKey
genPubKey = do
    seed <- Gen.bytes (Range.singleton 32)
    let (pubk, _) = createKeypairFromSeed seed
    return $ PublicKey pubk

-- | Generates a 'SecretKey'.
genSecKey :: Gen SecretKey
genSecKey = do
    seed <- Gen.bytes (Range.singleton 32)
    let (_, privk) = createKeypairFromSeed seed
    return $ SecretKey privk

-- | Generates a 'Address'.
genPubKeyAddr :: Gen Address
genPubKeyAddr = do
    bootBool <- Gen.bool
    pubk <- genPubKey
    return $ makePubKeyAddress (IsBootstrapEraAddr bootBool) pubk

-- | Generates a 'TxOut'.
genTxOut :: Gen TxOut
genTxOut = do
    address <- genPubKeyAddr
    coin <- Gen.word64 Range.constantBounded
    return $ TxOut address (Coin coin)

-- | Generates a 'NonEmpty' list of 'TxOut's.
genTxOutList :: Gen (NonEmpty TxOut)
genTxOutList = Gen.nonEmpty (Range.constant 1 10) genTxOut

-- | Generates a 'TxIn'.
genTxIn :: Gen TxIn
genTxIn = do
    pubKeyAddr <- genPubKey
    txIndex <- Gen.word32 (Range.constant 1 10)
    return $ TxInUtxo (unsafeHash pubKeyAddr) (txIndex)

-- | Generates a 'NonEmpty' list of 'TxIn's.
genTxInList :: Gen (NonEmpty TxIn)
genTxInList = Gen.nonEmpty (Range.constant 1 10) genTxIn

-- | Generates a 'Tx'.
genTx :: Gen Tx
genTx = do
    txIns <- Gen.nonEmpty (Range.constant 1 10) genTxIn
    txOuts <- Gen.nonEmpty (Range.constant 1 10) genTxOut
    return $ UnsafeTx txIns txOuts (mkAttributes ())

-- | Generates 'TxSigData'.
genTxSigData :: Gen TxSigData
genTxSigData = do
    tx <- genTx
    return $ TxSigData (unsafeHash tx)

-- | Generates a 'SignTag'.
genSignTag :: Gen SignTag
genSignTag =  Gen.element [ SignForTestingOnly
                          , SignTx
                          , SignRedeemTx
                          , SignVssCert
                          , SignUSProposal
                          , SignCommitment
                          , SignUSVote
                          , SignMainBlock
                          , SignMainBlockLight
                          , SignMainBlockHeavy
                          , SignProxySK
                          ]

-- | Generates a 'TxSig'.
genTxSig :: Gen TxSig
genTxSig = do
    pM <- Gen.int32 Range.constantBounded
    signTag <- genSignTag
    sk <- genSecKey
    txSigData <- genTxSigData
    return $ (sign (ProtocolMagic pM) signTag sk txSigData)

-- | Generates a 'TxInWitness' ('PkWitness').
genPkWitness :: Gen TxInWitness
genPkWitness = do
    pubk <- genPubKey
    txsig <- genTxSig
    return $ PkWitness pubk txsig

genScript :: Gen Script
genScript = do
    version <- Gen.word16 Range.constantBounded
    sScript <- Gen.bytes (Range.singleton 32) -- arbitrarily picked script serialization length
    return $ Script version sScript

-- | Generates a 'TxInWitness' ('ScriptWitness').
genScriptWitness :: Gen TxInWitness
genScriptWitness = do
    script1 <- genScript
    script2 <- genScript
    return $ ScriptWitness script1 script2

genRedPubKey :: Gen RedeemPublicKey
genRedPubKey = do
    bytes <- Gen.bytes (Range.singleton 32)
    return $ redeemPkBuild bytes

genRedSecKey :: Gen RedeemSecretKey
genRedSecKey = do
    bytes <- Gen.bytes (Range.singleton 32)
    return . snd $ fromJust (redeemDeterministicKeyGen bytes)

genRedSig :: Gen (RedeemSignature TxSigData)
genRedSig = do
    pMag <- Gen.int32 Range.constantBounded
    tag <- genSignTag
    redSecKey <- genRedSecKey
    txSigData <- genTxSigData
    return $ redeemSign (ProtocolMagic pMag) tag redSecKey txSigData

-- | Generates a 'TxInWitness' ('RedeemWitness').
genRedWitness :: Gen TxInWitness
genRedWitness = do
    redPubKey <- genRedPubKey
    redSig <- genRedSig
    return $ RedeemWitness redPubKey redSig

-- | Generates a 'TxInWitness' ('UnknownWitnessType').
genUnknownWitness :: Gen TxInWitness
genUnknownWitness = do
    word <- Gen.word8 (Range.constantBounded)
    bs <- Gen.bytes (Range.constant 0 50)
    return $ UnknownWitnessType word bs

----------------------------------------------------------------------------
-- Round Trip Tests
----------------------------------------------------------------------------

prop_round_trip_PkWitness :: Property
prop_round_trip_PkWitness =
    property $ (forAll genPkWitness) >>= trippingBiShow

prop_round_trip_ScriptWitness :: Property
prop_round_trip_ScriptWitness =
    property $ (forAll genScriptWitness) >>= trippingBiShow

prop_round_trip_RedeemWitness :: Property
prop_round_trip_RedeemWitness =
    property $ (forAll genRedWitness) >>= trippingBiShow

prop_round_trip_UnknownWitness :: Property
prop_round_trip_UnknownWitness =
    property $ (forAll genUnknownWitness) >>= trippingBiShow

----------------------------------------------------------------------------
-- Misc
----------------------------------------------------------------------------

-- TODO: add fst not null to the third condition, you can then remove then
-- remove the first.
hexFormatFunc :: LB.ByteString -> LB.ByteString
hexFormatFunc bs
    | LB.length bs <= 32 = bs
    | lengthOfRem >= 32  = (fst splitTupleBS `LB.append` "\n") `LB.append` (hexFormatFunc $ snd splitTupleBS)
    | lengthOfRem < 32   = snd splitTupleBS
    | otherwise          = bs
  where
    splitTupleBS = LB.splitAt 32 bs
    lengthOfRem  = (LB.length $ snd splitTupleBS)
