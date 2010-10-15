{-# LANGUAGE FlexibleInstances #-}
import Test.Framework (defaultMain)

import Data.Object.Json
import Data.Object
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit hiding (Test)
import Test.QuickCheck (Arbitrary (..), oneof)
import Control.Applicative
import Numeric
import qualified Data.ByteString.Char8 as S8

input :: String
input = "{\"this\":[\"is\",\"a\",\"sample\"],\"numbers\":[5,6,7890,\"456\",1234567890]}"

output :: StringObject
output = Mapping
            [ ("numbers", Sequence $ map Scalar
                            [ "5"
                            , "6"
                            , "7890"
                            , "456"
                            , "1234567890"
              ])
            , ("this", Sequence $ map Scalar ["is", "a", "sample"])
            ]

caseInputOutput :: Assertion
caseInputOutput = do
    so <- decode $ S8.pack input
    output @=? so

propInputOutput :: StringObject -> Bool
propInputOutput so = decode (encode so) == Just so

main :: IO ()
main = defaultMain
    [ testCase "caseInputOutput" caseInputOutput
    , testProperty "propInputOutput" propInputOutput
    , testProperty "propShowSignedInt" propShowSignedInt
    ]

instance Arbitrary (Object String String) where
    arbitrary = oneof
                    [ Scalar <$> arbitrary
                    , do
                        i <- arbitrary
                        let s = showSignedInt (i * 1000000000 :: Integer)
                        return $ Scalar s
                    , do
                        a <- arbitrary
                        b <- arbitrary
                        return $ Sequence [a, b]
                    , do
                        k1 <- arbitrary
                        v1 <- arbitrary
                        return $ Mapping [(k1, v1)]
                    ]

propShowSignedInt :: Integer -> Bool
propShowSignedInt i = read (showSignedInt i) == i

showSignedInt :: Integral a => a -> String
showSignedInt i
    | i < 0 = '-' : showSignedInt (negate i)
    | otherwise = showInt i ""
