{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Contract.Funds
    ( ownFunds
    , ownFunds'
    ) where

import           Control.Monad                  ( void )
import qualified Data.Map                      as Map
import           Data.Monoid                    ( Last(..) )
import           Data.Text                      ( Text )
import           Ledger                         ( TxOut(txOutValue)
                                                , TxOutTx(txOutTxOut)
                                                , Value
                                                , pubKeyAddress
                                                )
import qualified Ledger.Value                  as Value
import qualified Plutus.Contract               as Contract
import           PlutusTx.Prelude        hiding ( (<$>) )
import           Prelude                        ( (<$>) )

ownFunds :: Contract.HasBlockchainActions s => Contract.Contract w s Text Value
ownFunds = do
    pk    <- Contract.ownPubKey
    utxos <- Contract.utxoAt $ pubKeyAddress pk
    let v = mconcat $ Map.elems $ txOutValue . txOutTxOut <$> utxos
    Contract.logInfo @String $ "own funds: " ++ show (Value.flattenValue v)
    return v

ownFunds' :: Contract.Contract (Last Value) Contract.BlockchainActions Text ()
ownFunds' = do
    Contract.handleError Contract.logError
        $   ownFunds
        >>= Contract.tell
        .   Last
        .   Just
    void $ Contract.waitNSlots 1
    ownFunds'
