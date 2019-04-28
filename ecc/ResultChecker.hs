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
        p = read (head rawlines) :: Int
        c = read (rawlines !! 1) :: Int

--      nCandidates  rawvotes    (parsedVote)
parseVoter :: Int -> [String] -> Vote
parseVoter n (addr:rawVotes) = (addr,rawToCiphers rawCiphers)
    where
        rawCiphers = map (map read . words) rawVotes :: [[Integer]]

-- raw ciphers to cipher
rawToCiphers :: [[Integer]] -> [Cipher]
rawToCiphers = map (\rawVote -> (Coord (rawVote !! 0) (rawVote !! 1) ,Coord (rawVote !! 2) (rawVote !! 3) ))

readElectoralRegister :: String -> IO Register
readElectoralRegister path = do
    rawFile <- readFile path
    return $ parseRegister rawFile


slice :: Int -> Int -> [a] -> [a]
slice from to xs = take (to - from + 1) (drop from xs)


isValidVote :: PublicKey -> SecretKey -> Vote -> Bool
isValidVote pk sk (addr,ciphers) = 1 ==  decryptSumOfMessages pk sk ciphers 


--                 pk            sk         all votes      good      invalid
separateVotes :: PublicKey -> SecretKey -> Register -> IO (Register,Register)
separateVotes pk sk register  = return $ partition (isValidVote pk sk) register



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
    putStrLn ("Total people voted is " ++ show (length register))
    putStrLn "Filtering out invalid ballots"
    (valid,invalid) <- separateVotes pK sK register
    putStrLn (show (length invalid) ++ " invalid ballots found.")
    print (tallyVotes pK sK valid)


    


aggregate :: Num a => [a] -> [a] -> [a]
aggregate [] y = y 
aggregate x [] = x
aggregate x y = zipWith (+) x y


main :: IO ()
main = do
    rawInput <- getContents
    let register = parseRegister rawInput
    putStrLn ("# voters:\t" ++ show (length register))
    (valid,invalid) <- separateVotes pK sK register
    putStrLn ("# invalid: \t" ++ show (length invalid))
    print (tallyVotes pK sK valid)