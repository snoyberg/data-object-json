{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeSynonymInstances #-}
---------------------------------------------------------
--
-- Module        : Data.Object.Json
-- Copyright     : Michael Snoyman
-- License       : BSD3
--
-- Maintainer    : Michael Snoyman <michael@snoyman.com>
-- Stability     : Stable
-- Portability   : portable
---------------------------------------------------------

-- | A simple wrapper around the json-b library which presents values inside
-- 'Object's.
module Data.Object.Json
    ( -- * Types
      JsonDoc (..)
    , JsonScalar (..)
    , JsonObject
      -- * Serialization
    , JsonDecodeError (..)
    , decode
    , encode
      -- * Specialization
    , toJsonObject
    , fromJsonObject
    ) where

import Data.Object.Text
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Trie
import Control.Arrow
import Control.Applicative ((<$>))
import Data.Generics
import Control.Exception
import Data.Attempt
import Data.Convertible.Text

import Text.JSONb.Simple as J
import qualified Text.JSONb.Decode as Decode
import qualified Text.JSONb.Encode as Encode

-- | A fully formed JSON document.
newtype JsonDoc = JsonDoc { unJsonDoc :: BL.ByteString }
    deriving (Show, Eq)

-- | Matches the scalar data types used in json-b so we can have proper mapping
-- between the two libraries.
data JsonScalar =
    JsonString BS.ByteString
    | JsonNumber Rational
    | JsonBoolean Bool
    | JsonNull

instance ConvertSuccess JsonScalar Text where
    convertSuccess (JsonString b) = convertSuccess b
    convertSuccess (JsonNumber r) = convertSuccess r
    convertSuccess (JsonBoolean b) = convertSuccess b
    convertSuccess JsonNull = convertSuccess ""
instance ConvertSuccess Text JsonScalar where
    convertSuccess = JsonString . convertSuccess
instance ConvertAttempt JsonScalar Text where
    convertAttempt = return . convertSuccess
instance ConvertAttempt Text JsonScalar where
    convertAttempt = return . convertSuccess

-- | Meant to match closely with the 'JSON' data type. Therefore, uses strict
-- byte strings for keys and the 'JsonScalar' type for scalars.
type JsonObject = Object BS.ByteString JsonScalar

instance ToObject JSON BS.ByteString JsonScalar where
    toObject (J.Object trie) = toObject $ Data.Trie.toList trie
    toObject (J.Array a) = toObject a
    toObject (J.String bs) = Scalar $ JsonString bs
    toObject (J.Number r) = Scalar $ JsonNumber r
    toObject (J.Boolean b) = Scalar $ JsonBoolean b
    toObject J.Null = Scalar JsonNull
instance FromObject JSON BS.ByteString JsonScalar where
    fromObject (Scalar (JsonString bs)) = return $ J.String bs
    fromObject (Scalar (JsonNumber r)) = return $ J.Number r
    fromObject (Scalar (JsonBoolean b)) = return $ J.Boolean b
    fromObject (Scalar JsonNull) = return J.Null
    fromObject (Sequence s) = J.Array <$> mapM fromObject s
    fromObject (Mapping m) =
        J.Object . Data.Trie.fromList <$> mapM
        (runKleisli $ second $ Kleisli fromObject) m

-- | Error type for JSON decoding errors.
newtype JsonDecodeError = JsonDecodeError String
    deriving (Show, Typeable)
instance Exception JsonDecodeError

-- | Decode a lazy bytestring into any value that can be converted from a
-- 'JsonObject'. Be aware that both parsing and conversion errors will be
-- reflected in the 'MonadAttempt' wrapper; if you wish to receive those errors
-- separate, first use this function to decode to a 'JsonObject' and then
-- 'fromJsonObject' to perform the conversion.
decode :: (FromObject v BS.ByteString JsonScalar)
       => BL.ByteString
       -> Attempt v
decode = either (failure . JsonDecodeError . fst)
                (fromJsonObject . toJsonObject)
       . Decode.decode

-- | Encode any value which can be converted to a 'JsonObject' into a lazy
-- bytestring.
encode :: ToObject v BS.ByteString JsonScalar
       => v
       -> BL.ByteString
encode = Encode.encode Encode.Compact
       . fromSuccess
       . fromJsonObject
       . toJsonObject

-- | 'toObject' specialized for 'JsonObject's
toJsonObject :: ToObject a BS.ByteString JsonScalar => a -> JsonObject
toJsonObject = toObject

-- | 'fromObject' specialized for 'JsonObject's
fromJsonObject :: FromObject a BS.ByteString JsonScalar
               => JsonObject
               -> Attempt a
fromJsonObject = fromObject

instance ConvertSuccess JsonObject JsonDoc where
    convertSuccess = JsonDoc . encode
instance ConvertAttempt JsonDoc JsonObject where
    convertAttempt = decode . unJsonDoc
