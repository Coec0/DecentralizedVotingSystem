-- Program used to calculate the reuslt of point multiplication on an elliptic curve over a field of modulo of a prime number

-- Program is run as a command line tool 
-- Sample use:
-- ./elliptic a b p x y n

-- where a,b and p are used to specify the elliptic curve using the formula y^2 = x^3 + a*x + b
-- x,y are the coordinates of the point on the curve (The point MUST be on the curve)
-- n is the scalar to multiply with

-- OUTPUT: one line representing the result of (x,y)^n 

import Data.List
import Data.Bits

import System.Environment

-- Data type to represent a Point on the curve
data Point = Coord Integer Integer | Identity

-- A curve is represented with a,b and p where p MUST be a prime
type Curve = (Integer,Integer,Integer)


-- Overriding the show method
instance Show Point where
    show (Coord x y) = "("++ show x ++","++ show y ++")"
    show Identity  = "Identity"


-- Used to check the integer solutions for y^2 = x^3 + a*x + b (mod p)
findSolutionsonX :: Integer -> Curve -> [Point]
findSolutionsonX x (a,b,p) | x >= p = []
                           | otherwise = map (Coord x) possibleY ++ findSolutionsonX (x+1) (a,b,p)
                            where 
                                rhs = (x*x*x + a*x + b) `mod` p
                                lhs :: Integer -> Integer
                                lhs y = (y*y) `mod` p
                                possibleY = map fst $ filter (\y -> rhs == snd y) $ map (\y -> (y,(y*y) `mod` p)) [0..p-1]

findPtsOnCurve :: Curve -> [Point]
findPtsOnCurve (a,b,p) = findSolutionsonX 0 (a,b,p)


-- Given two points and a curve, returns whether P+Q = I
isInverse ::Curve -> Point -> Point -> Bool
isInverse _ Identity _ = False
isInverse _ _ Identity = False
isInverse (a,b,p) (Coord x1 y1) (Coord x2 y2) | x1 /= x2 = False
                                              | otherwise = y2 == ((-y1) `mod` p)

-- Given two points and a curve, returns the result of P+Q
pointAdd :: Curve -> Point -> Point -> Point
pointAdd _ Identity q = q
pointAdd _ p Identity = p
pointAdd c p q | isInverse c p q = Identity
pointAdd (a,b,p) (Coord x1 y1) (Coord x2 y2) = Coord rx ry
    where
        lambda = (y2 - y1) * invertMod (x2 - x1) p
        rx = (lambda*lambda - x1 - x2) `mod` p
        ry = (lambda * (x1  - rx ) - y1) `mod` p

-- Returns the result of 2P on a curve
pointDouble :: Curve -> Point -> Point
pointDouble _ Identity = Identity
pointDouble (a,b,p) (Coord x y) | y == 0 = Identity
                                | otherwise = Coord rx ry
    where
        lambda = (3*x*x+a)*invertMod (2*y) p 
        rx = ((lambda*lambda) - 2*x) `mod` p
        ry = (lambda * (x  - rx ) - y) `mod` p


-- The main method that returns nP
pointMul :: Curve -> Point -> Integer -> Point
pointMul _ _ 0 = Identity
pointMul _ p 1 = p
pointMul _ Identity _ = Identity
pointMul c p n | odd n = pointAdd c p (pointMul c p (n-1))
               | even n  = pointMul c (pointDouble c p) (n `div` 2)

-- Used to calculate the inverse modulo the prime according  to Fermat's Last Theorem
invertMod :: Integer -> Integer -> Integer
invertMod x p = modExp x (p-2) p

-- A generalization of InvertMod
modExp :: Integer -> Integer -> Integer -> Integer
modExp b 0 m = 1
modExp b e m = t * modExp ((b * b) `mod` m) (shiftR e 1) m `mod` m
    where t = if testBit e 0 then b `mod` m else 1

-- The main program
main :: IO ()    
main = do
    args <- getArgs
    let a = read (args !! 0) :: Integer
    let b = read (args !! 1) :: Integer
    let p = read (args !! 2) :: Integer
    
    let x = read (args !! 3) :: Integer
    let y = read (args !! 4) :: Integer
 
    let n = read (args !! 5) :: Integer

    let r = pointMul (a,b,p) (Coord x y) n

    print r

