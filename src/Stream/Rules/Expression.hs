module Stream.Rules.Expression where

import qualified Data.IntMap as M

import Program.CST
import Stream.Types
import Stream.Verdict
import Data.Function (on)

type StreamDMap = M.IntMap StreamD


evalExpr :: Expr -> Int -> StreamDMap -> (StreamDMap, Int)

-- Base Cases: Constant, Member & Time
evalExpr (Val vType) key streamDEnv = (M.insert key f streamDEnv, key + 1)
    where
        f = valType vType

        valType (VNum v) = \_ _ _ -> Num v
        valType (VStr v) = \_ _ _ -> Str v
        valType VTime = \_ _ t' -> Num t'
        valType (Member Name) = \_ m _ -> maybe undefined (\(n,_,_) -> Str n) m
        valType (Member Power) = \_ m _ -> maybe undefined (\(_,p,_) -> Num p) m
        valType (Member Active) = \_ m _ -> maybe undefined (\(_,_,a) -> Ver a) m

-- Binary Operation
evalExpr (BinOp binOp e1 e2) k streamDEnv = (M.insert k f env2, k'')
    where
        (env1, k') = evalExpr e1 (k+1) streamDEnv
        (env2, k'') = evalExpr e2 k' env1

        s1 = env2 M.! (k+1)
        s2 = env2 M.! k'

        f t d t' = streamOp binOp (s1 t d t') (s2 t d t')

        streamOp Plus = (+)
        streamOp Minus = (-)
        streamOp LogicalOr = (Ver .) . (|||) `on` getVerUnsafe
        streamOp LessThan = ((Ver . asVerdict) .) . (<) `on` getNumUnsafe

-- Unary
evalExpr (UnOp op e) k env = (M.insert k f env', k')
    where
        (env', k') = evalExpr e (k+1) env

        s1 = env' M.! (k+1)

        f = ((streamOp op .) .) . s1

        streamOp Negate = negate
        streamOp LogicalNot = Ver . nnot . getVerUnsafe


-- Expr Always & Eventually
evalExpr (MTLExpr mtl bounds e) k env = (M.insert k f env', k')
    where
        (env', k') = evalExpr e (k+1) env

        verdictLogic Always = vConjunction 
        verdictLogic Eventually = vDisjunction 
        
        isFinalVerdict Always p = if p then Undecided else TTrue 
        isFinalVerdict Eventually p = if p then Undecided else FFalse 

        f t d t' = Ver giveVerdict
            where
                s1 = env' M.! (k+1)

                (a, b) = case bounds of 
                    None -> (0, t'+1)
                    Range r1 r2 -> (r1, r2)

                lower = a + t
                upper = min t' (t + b)

                finalVer = isFinalVerdict mtl (t' < t + b)
                giveVerdict = verdictLogic mtl (finalVer : [getVerUnsafe (s1 i d t') | i <- [lower..upper]])
