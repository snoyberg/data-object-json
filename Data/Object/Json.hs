{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
---------------------------------------------------------
--
-- Module        : Data.Object.Json
-- Copyright     : Michael Snoyman
-- License       : BSD3
--
-- Maintainer    : Michael Snoyman <michael@snoyman.com>
-- Stability     : Stable
-- Portability   : portable


-- | A simple wrapper around the json-b library which presents values inside
-- 'Object's.
module Data.Object.Json
    ( -- * Definition of 'JsonObject'
      JsonScalar (..)
    , JsonObject
      -- * Automatic scalar conversions
    , IsJsonScalar (..)
    , toJsonObject
    , fromJsonObject
      -- * Encoding/decoding
    , encode
    , encodeFile
    , decode
    , decodeFile
    ) where

import Data.Object
import qualified Data.Trie
import Control.Arrow
import Data.Data
import Control.Exception

import Text.JSONb.Simple as J
import qualified Text.JSONb.Decode as Decode
import qualified Text.JSONb.Encode as Encode

import qualified Data.Text
import qualified Data.Text.Lazy

import qualified Data.ByteString
import qualified Data.ByteString.Lazy

import Data.Convertible.Text (ConvertSuccess, cs)
import Control.Failure

-- | Matches the scalar data types used in json-b so we can have proper mapping
-- between the two libraries.
data JsonScalar =
    JsonString Data.ByteString.ByteString
    | JsonNumber Rational
    | JsonBoolean Bool
    | JsonNull
    deriving (Eq, Show, Read, Data, Typeable)

jsToBS :: JsonScalar -> Data.ByteString.ByteString
jsToBS (JsonString b) = b
jsToBS (JsonNumber r) = cs r
jsToBS (JsonBoolean b) = cs b
jsToBS JsonNull = Data.ByteString.empty

class (Eq a) => IsJsonScalar a where
    fromJsonScalar :: JsonScalar -> a
    toJsonScalar :: a -> JsonScalar
instance IsJsonScalar JsonScalar where
    fromJsonScalar = id
    toJsonScalar = id
instance IsJsonScalar Data.Text.Text where
    fromJsonScalar = cs . jsToBS
    toJsonScalar = JsonString . cs
instance IsJsonScalar Data.Text.Lazy.Text where
    fromJsonScalar = cs . jsToBS
    toJsonScalar = JsonString . cs
instance IsJsonScalar [Char] where
    fromJsonScalar = cs . jsToBS
    toJsonScalar = JsonString . cs
instance IsJsonScalar Data.ByteString.ByteString where
    fromJsonScalar = jsToBS
    toJsonScalar = JsonString
instance IsJsonScalar Data.ByteString.Lazy.ByteString where
    fromJsonScalar = cs . jsToBS
    toJsonScalar = JsonString . cs

toJsonObject :: ConvertSuccess k Data.ByteString.ByteString
             => IsJsonScalar v
             => Object k v
             -> JsonObject
toJsonObject = mapKeysValues cs toJsonScalar

fromJsonObject :: ConvertSuccess Data.ByteString.ByteString k
               => IsJsonScalar v
               => JsonObject
               -> Object k v
fromJsonObject = mapKeysValues cs fromJsonScalar

-- | Meant to match closely with the 'JSON' data type. Therefore, uses strict
-- byte strings for keys and the 'JsonScalar' type for scalars.
type JsonObject = Object Data.ByteString.ByteString JsonScalar

jsonToJO :: JSON -> JsonObject
jsonToJO (J.Object trie) =
    Mapping . map (second jsonToJO) $ Data.Trie.toList trie
jsonToJO (J.Array a) = Sequence $ map jsonToJO $ a
jsonToJO (J.String bs) = Scalar $ JsonString bs
jsonToJO (J.Number r) = Scalar $ JsonNumber r
jsonToJO (J.Boolean b) = Scalar $ JsonBoolean b
jsonToJO J.Null = Scalar JsonNull

joToJSON :: JsonObject -> JSON
joToJSON (Scalar (JsonString bs)) = J.String bs
joToJSON (Scalar (JsonNumber r)) = J.Number r
joToJSON (Scalar (JsonBoolean b)) = J.Boolean b
joToJSON (Scalar JsonNull) = J.Null
joToJSON (Sequence s) = J.Array $ map joToJSON s
joToJSON (Mapping m) =
    J.Object $ Data.Trie.fromList $ map (second joToJSON) m

-- | Error type for JSON decoding errors.
newtype JsonDecodeError = JsonDecodeError String
    deriving (Show, Typeable)
instance Exception JsonDecodeError

-- | Decode a lazy bytestring into a 'JsonObject'.
decode :: Failure JsonDecodeError m
       => ConvertSuccess Data.ByteString.ByteString k
       => IsJsonScalar v
       => Data.ByteString.ByteString
       -> m (Object k v)
decode = either (failure . JsonDecodeError)
                (return . fromJsonObject . jsonToJO)
       . Decode.decode

-- | Encode a 'JsonObject' into a lazy bytestring.
encode :: (ConvertSuccess k Data.ByteString.ByteString, IsJsonScalar v)
       => Object k v
       -> Data.ByteString.ByteString
encode = Encode.encode Encode.Compact . joToJSON . toJsonObject

encodeFile :: (ConvertSuccess k Data.ByteString.ByteString, IsJsonScalar v)
           => FilePath
           -> Object k v
           -> IO ()
encodeFile fp = Data.ByteString.writeFile fp . encode

decodeFile :: Failure JsonDecodeError m
           => ConvertSuccess Data.ByteString.ByteString k
           => IsJsonScalar v
           => FilePath
           -> IO (m (Object k v))
decodeFile fp = decode `fmap` Data.ByteString.readFile fp
