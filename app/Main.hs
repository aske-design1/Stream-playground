module Main where
import Monitor (monitor)
import Program.CST 

import Data.Sequence as S ( fromList )
import Stream.Verdict ( Verdict(TTrue) )
import Stream.CSTConversion (convertCSTToStreams)

main :: IO ()
main = monitor env time devices True
    where
        -- Create Expression
        n = Val . VNum
        expr = BinOp LessThan (Val VTime) (n 50)
        prog = Prop Always None expr

        env = convertCSTToStreams prog

        -- How long should it run for? 
        time = [0..75]

        -- Device Logic:
        devices = S.fromList [[("Roomba", x * mult, TTrue)] | x <- time, let mult = if even x then 20 else 10]


