module Elgamal where

import EllipticAlgebra
import System.Random
import Data.Maybe
import System.Entropy
import Data.Binary.Get
import qualified Data.ByteString.Lazy as B
 

--                  G    ord(g)  g     beta=d.g
type PublicKey = (Curve,Integer,Point,Point)

--                d
type SecretKey = Integer



generateKeys :: Curve -> Integer -> (PublicKey,SecretKey)
generateKeys c@(a,b,p) seed = (pk,d)
    where 
        pk :: PublicKey
        lstOfG = (getGenerators c)
        g = lstOfG <> (seed `mod` (sizeOf lstOfG))
        d = (seed `mod` p) :: Integer
        pk = (c,fromIntegral $ getOrderOfGenerator c g,g,pointMul c g d)


(<>) :: [a] -> Integer -> a
(<>) [] _ = error "Empty list"
(<>) (x:_) 0 = x
(<>) (_:xs) n = xs <> (n-1)

sizeOf :: [a] -> Integer
sizeOf [] = 0
sizeOf (x:xs) = 1 + sizeOf(xs)



