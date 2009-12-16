{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TemplateHaskell #-}
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
      -- * IO
    , readJsonDoc
    , writeJsonDoc
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

readJsonDoc :: FilePath -> IO JsonDoc
readJsonDoc = fmap JsonDoc . BL.readFile

writeJsonDoc :: FilePath -> JsonDoc -> IO ()
writeJsonDoc fp = BL.writeFile fp . unJsonDoc

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

instance ConvertSuccess JsonScalar String where
    convertSuccess = cs . (cs :: JsonScalar -> Text)
instance ConvertSuccess String JsonScalar where
    convertSuccess = JsonString . cs

instance ConvertSuccess JsonScalar BS.ByteString where
    convertSuccess = cs . (cs :: JsonScalar -> Text)
instance ConvertSuccess BS.ByteString JsonScalar where
    convertSuccess = JsonString . cs

$(let types = [''String, ''BS.ByteString, ''Text]
   in deriveAttempts $
        [(k, ''JsonScalar) | k <- types] ++
        [(''JsonScalar, v) | v <- types])

$(deriveSuccessConvs ''BS.ByteString ''JsonScalar
    [''String, ''BS.ByteString, ''Text]
    [''String, ''BS.ByteString, ''Text, ''JsonScalar])

-- | Meant to match closely with the 'JSON' data type. Therefore, uses strict
-- byte strings for keys and the 'JsonScalar' type for scalars.
type JsonObject = Object BS.ByteString JsonScalar

instance ConvertSuccess JSON JsonObject where
    convertSuccess (J.Object trie) =
        Mapping . map (second cs) $ Data.Trie.toList trie
    convertSuccess (J.Array a) = Sequence $ map cs $ a
    convertSuccess (J.String bs) = Scalar $ JsonString bs
    convertSuccess (J.Number r) = Scalar $ JsonNumber r
    convertSuccess (J.Boolean b) = Scalar $ JsonBoolean b
    convertSuccess J.Null = Scalar JsonNull
instance ConvertAttempt JsonObject JSON where
    convertAttempt (Scalar (JsonString bs)) = return $ J.String bs
    convertAttempt (Scalar (JsonNumber r)) = return $ J.Number r
    convertAttempt (Scalar (JsonBoolean b)) = return $ J.Boolean b
    convertAttempt (Scalar JsonNull) = return J.Null
    convertAttempt (Sequence s) = J.Array <$> mapM ca s
    convertAttempt (Mapping m) =
        J.Object . Data.Trie.fromList <$> mapM
        (runKleisli $ second $ Kleisli ca) m

-- | Error type for JSON decoding errors.
newtype JsonDecodeError = JsonDecodeError String
    deriving (Show, Typeable)
instance Exception JsonDecodeError

-- | Decode a lazy bytestring into a 'JsonObject'.
decode :: MonadFailure JsonDecodeError m
       => BL.ByteString
       -> m JsonObject
decode = either (failure . JsonDecodeError . fst)
                (return . toJsonObject)
       . Decode.decode

-- | Encode a 'JsonObject' into a lazy bytestring.
encode :: JsonObject
       -> BL.ByteString
encode = Encode.encode Encode.Compact
       . fromSuccess
       . fromJsonObject

-- | 'convertSuccess' specialized for 'JsonObject's
toJsonObject :: ConvertSuccess a JsonObject => a -> JsonObject
toJsonObject = cs

-- | 'convertAttempt' specialized for 'JsonObject's
fromJsonObject :: ConvertAttempt JsonObject a
               => JsonObject
               -> Attempt a
fromJsonObject = ca

instance ConvertSuccess JsonObject JsonDoc where
    convertSuccess = JsonDoc . encode
instance ConvertAttempt JsonDoc JsonObject where
    convertAttempt = decode . unJsonDoc
