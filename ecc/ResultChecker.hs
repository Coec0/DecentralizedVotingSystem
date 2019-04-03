module ResultChecker where

import Elgamal
import EllipticAlgebra
import Data.List.Split
import Data.List

type Vote = (String,[Cipher])

type Register = [Vote]

--             p       c      rawInfo
parseVoters :: Int -> Int -> [String] -> Register
parseVoters p c raw = map (parseVoter c)(chunksOf (c+1) raw) 


parseRegister :: String -> Register
parseRegister raw = parseVoters p c (drop 2 rawlines)
    where
        rawlines = lines raw :: [String]
        p = read (rawlines !! 0) :: Int
        c = read (rawlines !! 1) :: Int

--      nCandidates  rawvotes    (parsedVote)
parseVoter :: Int -> [String] -> Vote
parseVoter n (addr:rawVotes) = (addr,rawToCiphers rawCiphers)
    where
        rawCiphers = map (map read) (map words rawVotes) :: [[Integer]]

-- raw ciphers to cipher
rawToCiphers :: [[Integer]] -> [Cipher]
rawToCiphers [] = []
rawToCiphers (rawVote:rawVotes) =  ((Coord (rawVote !! 0) (rawVote !! 1) ),(Coord (rawVote !! 2) (rawVote !! 3))): rawToCiphers rawVotes

readElectoralRegister :: String -> IO Register
readElectoralRegister path = do
    rawFile <- readFile path
    return $ parseRegister rawFile


slice :: Int -> Int -> [a] -> [a]
slice from to xs = take (to - from + 1) (drop from xs)


isValidVote :: PublicKey -> SecretKey -> Vote -> Bool
isValidVote pk sk (addr,ciphers) = 1 ==  decryptSumOfMessages pk sk ciphers 


--                 pk            sk         all votes      good      invalid
separateVotes :: PublicKey -> SecretKey -> Register -> (Register,Register)
separateVotes pk sk register = partition (isValidVote pk sk) register 



-- tally results
tallyVotes :: PublicKey -> SecretKey -> Register -> [Integer]
tallyVotes _ _ [] = []
tallyVotes pk sk (p@(_,votes):ps) = aggregate (getPersonVotes pk sk votes) (tallyVotes pk sk ps)

getPersonVotes :: PublicKey -> SecretKey -> [Cipher] -> [Integer]
getPersonVotes _ _ [] = []
getPersonVotes pk sk (c:cs) = decryptMessage pk sk c:getPersonVotes pk sk cs


testKeys :: (PublicKey,SecretKey)
testKeys = (((49,109,541),61,(Coord 153 308),(Coord 419 248)),19)

pK = fst testKeys
sK = snd testKeys


getElectionResult :: String -> IO ()
getElectionResult path = do
    register <- readElectoralRegister path
    print ("Total pepople voted is " ++ show (length register))
    let  (valid,invalid) = separateVotes pK sK register
    print ("Number of invalid votes is " ++ show (length invalid) )
    print (tallyVotes pK sK valid)


aggregate :: Num a => [a] -> [a] -> [a]
aggregate [] y = y 
aggregate x [] = x
aggregate x y = zipWith (+) x y