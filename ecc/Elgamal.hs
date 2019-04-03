module Elgamal(PublicKey,SecretKey,Cipher,decryptMessage,decryptSumOfMessages) where

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

type Cipher = (Point,Point)


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
testCurve = (49,109,541)

testKeys :: (PublicKey,SecretKey)
testKeys = (((49,109,541),61,(Coord 153 308),(Coord 419 248)),19)


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
        (pk@(_,q,_,_),sk) = testKeys
        m = messageSeed `mod` q
        cipher = encryptMessage pk pointSeed m
        m' = decryptMessage pk sk cipher



decryptSumOfMessages :: PublicKey -> SecretKey -> [(Point,Point)] -> Integer
decryptSumOfMessages pk@(c,_,_,_) sk ciphers = decryptMessage pk sk summation 
        where
            summation = foldr (\c1@(r1,t1) c2@(r2,t2) -> (pointAdd c r1 r2,pointAdd c t1 t2) ) (Identity,Identity) ciphers

--                     message   random k
prop_homomorphism :: [(Integer,Integer)] -> Bool
prop_homomorphism nums = expectedResult == calculatedResult
    where
        (pk@(_,q,_,_),sk) = testKeys
        numsSanitized = map (\(n,rand) -> (abs n `mod` q,abs rand)) nums 
        expectedResult = sum (map fst numsSanitized) `mod` q
        encryptedMessages = map (\(m,rand) -> encryptMessage pk rand m) numsSanitized    :: [(Point,Point)]
        calculatedResult = decryptSumOfMessages pk sk encryptedMessages