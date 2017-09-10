module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Foreign.Class (class Decode, class Encode)
import Data.Foreign.Generic (defaultOptions, genericDecode, genericEncode)
import Data.Foreign.Generic (decodeJSON, encodeJSON)
import Data.Generic.Rep as Rep
import Data.Generic.Rep.Show (genericShow)
import Global.Unsafe (unsafeStringify)


newtype MyRecord = MyRecord {a :: Int}
derive instance genericMyRecord :: Rep.Generic MyRecord _
instance showMyRecord :: Show MyRecord where
  show = genericShow
instance decodeMyRecord :: Decode MyRecord where
  decode = genericDecode $ defaultOptions {unwrapSingleConstructors = true}
instance encodeMyRecord :: Encode MyRecord where
  encode = genericEncode $ defaultOptions {unwrapSingleConstructors = true}


main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  log "Hello sailor!"
  log $ show $ encodeJSON (MyRecord { a: 1})
