module Elgamal where

import EllipticAlgebra

import Test.QuickCheck
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
        lstOfG = getGenerators c
        g = lstOfG <> (seed `mod` sizeOf lstOfG)
        q = getOrderOfGenerator c g
        d = (pickSecret q p seed `mod` p) :: Integer
        pk = (c,q,g,pointMul c g d)

pickSecret :: Integer -> Integer -> Integer -> Integer
pickSecret q p s | (s `mod` q /= 0) && (s `mod` p /= 0) = s
                 | otherwise  = pickSecret q p (s+1)

--           Pk of the recepient  random    message  ciphertext
encryptMessage :: PublicKey -> Integer -> Integer ->(Point,Point)
encryptMessage pk@(c,q,g,beta) rand  m = (r,t)
    where 
        k = rand `mod` q
        r = pointMul c g k 
        t = pointAdd c (pointMul c g m) (pointMul c beta k)

--               pk recepient   sk recepient  ciphertext     point
decryptMessage :: PublicKey -> SecretKey -> (Point,Point) -> Integer       
decryptMessage pk@(c,q,g,beta) d (r,t) = fst $ head $ filter (\candidate -> snd candidate == point) $ map (\n -> (n,pointMul c g n)) [0..]
    where
         point = pointAdd c t (pointInvert c (pointMul c r d))


(<>) :: [a] -> Integer -> a
(<>) [] _ = error "Empty list"
(<>) (x:_) 0 = x
(<>) (_:xs) n = xs <> (n-1)

sizeOf :: [a] -> Integer
sizeOf = foldr (\x -> (+) 1) 0



testCurve :: (Integer,Integer,Integer)
testCurve = (1,1,109)

testKeys :: (PublicKey,SecretKey)
testKeys = generateKeys testCurve 4



simpleD :: PublicKey -> SecretKey -> (Point,Point) -> Point       
simpleD pk@(c,q,g,beta) d (r,t) = point
    where
         point = pointAdd c t (pointInvert c (pointMul c r d))


prop_encryptDecrypt :: Integer -> Integer -> Integer -> Bool
prop_encryptDecrypt keySeed' pointSeed' messageSeed' = m == m'
    where
        keySeed = 1 + keySeed' 
        pointSeed = 1 +  abs pointSeed'
        messageSeed = 1 +  abs messageSeed'
        (pk@(_,q,_,_),sk) = generateKeys testCurve keySeed
        m = messageSeed `mod` q
        cipher = encryptMessage pk pointSeed m
        m' = decryptMessage pk sk cipher



decryptSumOfMessages :: PublicKey -> SecretKey -> [(Point,Point)] -> Integer
decryptSumOfMessages pk@(c,_,_,_) sk ciphers = decryptMessage pk sk summation 
        where
            summation = foldr (\c1@(r1,t1) c2@(r2,t2) -> (pointAdd c r1 r2,pointAdd c t1 t2) ) (Identity,Identity) ciphers
