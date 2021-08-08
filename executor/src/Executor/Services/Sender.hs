module Executor.Services.Sender 
    ( SenderService(..)
    , mkSenderService
    ) where

import RIO
import Plutus.V1.Ledger.Tx
import Prelude (print)
import Cardano.Api.TxBody as Core

data SenderService = SenderService
    { send :: Tx -> IO () }

mkSenderService :: SenderService
mkSenderService = SenderService send'

send' :: Tx -> IO () 
send' tx = print "Submittings next tx to the network..." >> pure ()

fromPlutusTx :: Tx -> Core.Tx
fromPlutusTx Tx{..} =
    let coreTxIn = map txInputs (\TxIn{..} -> Core.TxIn txOutRefId txOutRefIdx)
        coreTxInsCollateral = Core.TxInsCollateral coreTxIn
        coreTxOuts = map txOutputs (\TxOut{..} -> Core.TxOut txOutAddress txOutValue txOutDatumHash)
        coreFee = TxFee txFee
        coreTxValidityRange = (getSlot txValidityRange, getSlot txValidityRange) -- ???
        coreTxMintValue = 
        coreTxBody = Core.TxBodyContent {
            txIns = coreTxIn,
            txInsCollateral = coreTxInsCollateral.
            txOuts = coreTxOuts,
            txFee = coreFee,
            txValidityRange = coreTxValidityRange,
            txMetadata = TxMetadataNone,
            txAuxScripts = TxAuxScriptsNone,
            txExtraScriptData = TxExtraScriptDataNone,
            txExtraKeyWits = TxExtraKeyWitnessesNone
            txProtocolParams = ???,
            txWithdrawals = TxWithdrawalsNone,
            txCertificates = TxCertificatesNone,
            txUpdateProposal = TxUpdateProposalNone,
            txMintValue = TxMintValue txMint ??? -- ???
        }
        coreTx = Core.makeShelleyTransactionBody coreTxBody
        coreWitness = Core.makeShelleyKeyWitness coreTxBody WitnessPaymentKey
        coreSigned = Core.makeSignedTransaction coreWitness coreTx
        coreTxBodyScriptData = Core.TxBodyScriptData txData txRedeemers
        shalleyBody = ShelleyTxBody coreSigned [] coreTxBodyScriptData Nothing
    serialiseShelleyBasedTx shalleyBody