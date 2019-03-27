import EllipticAlgebra

import System.Random
import Data.Maybe
import System.Entropy
import Data.Binary.Get
import qualified Data.ByteString.Lazy as B
 

type EncryptionInfo = (Curve,Point)
type Secret = (EncryptionInfo, Integer)


buildElgamal :: Curve -> Point -> Maybe EncryptionInfo
buildElgamal c g = if isPointOnCurve c g && isPrime (getOrderOfGenerator c g) then Just (c,g) else Nothing




generateSecret :: EncryptionInfo -> Integer -> Secret
generateSecret info@(c@(a,b,p),g) d = (info, d `mod` p)



isPrime k = null [ x | x <- [2..k - 1], k `mod` x == 0]