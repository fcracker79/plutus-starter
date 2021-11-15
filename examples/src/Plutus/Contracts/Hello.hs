{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE DataKinds                  #-}

module Plutus.Contracts.Hello where

import qualified Data.Text           as T
import           Playground.Contract
import           Plutus.Contract
import           PlutusTx.Prelude
import qualified Prelude             as Haskell
import qualified Data.ByteString.Short as SBS
import qualified Data.ByteString.Lazy as LBS



-- | A 'Contract' that logs a message.
hello :: Contract () EmptySchema T.Text ()
hello = logInfo @Haskell.String "Hello, world"


endpoints :: Contract () EmptySchema T.Text ()
endpoints = hello

-- 'mkSchemaDefinitions' doesn't work with 'EmptySchema'
-- (that is, with 0 endpoints) so we define a
-- dummy schema type with 1 endpoint to make it compile.
-- TODO: Repair 'mkSchemaDefinitions'
type DummySchema = Endpoint "dummy" ()

mkSchemaDefinitions ''DummySchema

$(mkKnownCurrencies [])
