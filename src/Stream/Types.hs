module Stream.Types where

import Stream.Verdict ( Verdict, Verdict(TTrue) )

import qualified Data.IntMap.Strict as M

data SOutput = Str String | Ver Verdict | Num Int
    deriving (Show, Eq)

instance Num SOutput where
    (Num a) + (Num b) = Num (a + b)
    _ + _ = undefined

    (Num a) - (Num b) = Num (a - b)
    _ - _ = undefined

    (Num a) * (Num b) = Num (a * b)
    (Ver a) * b
        | a == TTrue = b
        | otherwise = Num 0
    a * (Ver b) 
        | b == TTrue = a
        | otherwise = Num 0
    _ * _ = undefined

    abs = undefined
    signum (Num a) = Num (signum a)
    signum _ = undefined
    
    fromInteger = Num . fromIntegral

    negate (Num a) = Num (negate a)
    negate _ = undefined



-- Types
type EIoT = Maybe (String, Int, Verdict)

type StreamD = Int -> EIoT -> Int -> SOutput
type StreamO = Int -> Verdict
type StreamDI = Int -> M.IntMap Verdict

type Env = (M.IntMap StreamO, M.IntMap StreamDI, M.IntMap StreamD)


-- Helper Functions
getVerSafe :: SOutput -> Maybe Verdict
getVerSafe (Ver v) = Just v
getVerSafe _ = Nothing

getStrSafe :: SOutput -> Maybe String
getStrSafe (Str v) = Just v
getStrSafe _ = Nothing

getNumSafe :: SOutput -> Maybe Int
getNumSafe (Num v) = Just v
getNumSafe _ = Nothing

getVerUnsafe :: SOutput -> Verdict
getVerUnsafe (Ver v) = v
getVerUnsafe _ = undefined

getStrUnsafe :: SOutput -> String
getStrUnsafe (Str v) = v
getStrUnsafe _ = undefined

getNumUnsafe :: SOutput -> Int
getNumUnsafe (Num v) = v
getNumUnsafe _ = undefined
