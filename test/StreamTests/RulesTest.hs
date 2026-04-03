module StreamTests.RulesTest where

import Test.HUnit
import Program.CST


import qualified Data.IntMap as M
import Stream.Rules.Expression
import Stream.Types
import Data.IntMap
import Stream.Verdict (Verdict(FFalse, TTrue, Undecided))

initExprEval expr = evalExpr expr 0 M.empty

s idx = (M.! idx) . fst . initExprEval

testTimeVal = TestCase $
    let
        eval = Val VTime
        (funcMap, _) = evalExpr eval 0 M.empty
    in
        assertEqual "Time at 2 should return 2" ((funcMap M.! 0) 0 Nothing 2) (Num 2) >>
        assertEqual "Time at 5 should return 5" ((funcMap M.! 0) 3 Nothing 5) (Num 5)

testConstantVal = TestCase $
    let
        (funcMap1, _) = evalExpr (Val (VNum 30)) 0 M.empty
        (funcMap2, _) = evalExpr (Val (VNum 45)) 1 funcMap1
        (funcMap3, _) = evalExpr (Val (VStr "Hello World!")) 2 funcMap2
    in
        assertEqual "Should return 30" ((funcMap3 M.! 0) 0 Nothing 25) (Num 30) >>
        assertEqual "Should return 30" ((funcMap3 M.! 0) 2 Nothing 50) (Num 30) >>
        assertEqual "Should return 45" ((funcMap3 M.! 1) 0 Nothing 670) (Num 45) >>
        assertEqual "Should return \"Hello World!\"" ((funcMap3 M.! 2) 0 Nothing 3000) (Str "Hello World!")

--todo: Member

testBinaryOperationPlus = TestCase $
    let
        valExpr = Val (VNum 30)
        expr = BinOp Plus valExpr valExpr

    in
        assertEqual "Should return 30" (s 1 expr 0 Nothing 25) (Num 30) >>
        assertEqual "Should return 30" (s 2 expr 0 Nothing 25) (Num 30) >>
        assertEqual "Should return 60" (s 0 expr 0 Nothing 25) (Num 60)

testBinaryOperationLogicalOr = TestCase $
    let
        lt = BinOp LessThan
        numVal = Val . VNum
        tt = lt (numVal 0) (numVal 5)
        ff = lt (numVal 5) (numVal 5)

        expr1 = BinOp LogicalOr ff ff
        expr2 = BinOp LogicalOr ff tt
    in
        assertEqual "Should be false" (s 0 expr1 0 Nothing 25) (Ver FFalse) >>
        assertEqual "Should be true" (s 0 expr2 0 Nothing 25) (Ver TTrue)

testStreamAmount = TestCase $
    let
        getKey = snd . initExprEval

        lt = BinOp LessThan
        numVal = Val . VNum
        tt = lt (numVal 0) (numVal 5)

        expr = BinOp LogicalOr tt tt
    in 
        assertEqual "Expect 3" 3 (getKey tt) >>
        assertEqual "Expect 7" 7 (getKey expr) 

testUnaryOperationLogicalNot = TestCase $
    let
        lt = BinOp LessThan
        numVal = Val . VNum
        tt = lt (numVal 0) (numVal 5)

        notExpr = UnOp LogicalNot

        expr1 = notExpr tt
        expr2 = notExpr $ notExpr tt
    in
        assertEqual "Should be false" (s 0 expr1 0 Nothing 25) (Ver FFalse) >>
        assertEqual "Should be true" (s 0 expr2 0 Nothing 25) (Ver TTrue)

testUnaryOperationNegate = TestCase $
    let 
        numVal = Val . VNum
        negateExpr = UnOp Negate . numVal

        expr1 = negateExpr 30
        expr2 = negateExpr (-20)
    in
        assertEqual "Should be false" (s 0 expr1 0 Nothing 25) (Num (-30)) >>
        assertEqual "Should be true" (s 0 expr2 0 Nothing 25) (Num 20)

testAlwaysExpression = TestCase $
    let
        gUnbounded = MTLExpr Always None
        gBounded = MTLExpr Always (Range 0 30)

        lt = BinOp LessThan
        numVal = Val . VNum
        boolExpr = lt (Val VTime) (numVal 40)

        expr1 = gUnbounded boolExpr
        expr2 = gBounded boolExpr

    in
        assertEqual "Should be undecided" (s 0 expr1 0 Nothing 39) (Ver Undecided) >>
        assertEqual "Should be False" (s 0 expr1 0 Nothing 45) (Ver FFalse) >>
        assertEqual "Should be False" (s 0 expr1 40 Nothing 45) (Ver FFalse) >>
        
        assertEqual "Should be true" (s 0 expr2 0 Nothing 31) (Ver TTrue) >>
        assertEqual "Should be Undecided" (s 0 expr2 10 Nothing 39) (Ver Undecided) >>
        assertEqual "Should be False" (s 0 expr2 20 Nothing 49) (Ver FFalse) >>
        assertEqual "Should be False" (s 0 expr2 20 Nothing 60) (Ver FFalse)

testEventuallyExpression = TestCase $
    let
        gUnbounded = MTLExpr Eventually None
        gBounded = MTLExpr Eventually (Range 0 30)

        lt = BinOp LessThan
        numVal = Val . VNum
        boolExpr = lt (numVal 39) (Val VTime)

        expr1 = gUnbounded boolExpr
        expr2 = gBounded boolExpr

    in
        assertEqual "Should be undecided" (Ver Undecided) (s 0 expr1 0 Nothing 39)  >>
        assertEqual "Should be True"      (Ver TTrue)     (s 0 expr1 0 Nothing 40)  >>
        assertEqual "Should be True"      (Ver TTrue)     (s 0 expr1 39 Nothing 40) >>

        assertEqual "Should be False"     (Ver FFalse)    (s 0 expr2 0 Nothing 30)  >>
        assertEqual "Should be Undecided" (Ver Undecided) (s 0 expr2 10 Nothing 39) >>
        assertEqual "Should be True"      (Ver TTrue)     (s 0 expr2 10 Nothing 40) >>
        assertEqual "Should be True"      (Ver TTrue)     (s 0 expr2 20 Nothing 60)


rulesTest :: Test
rulesTest = TestList
    [
        TestLabel "Time Constant Test" testTimeVal,
        TestLabel "Constant Val Test" testConstantVal,
        TestLabel "Binary Operation Plus Test" testBinaryOperationPlus,
        TestLabel "Binary Operation Logical Or Test" testBinaryOperationLogicalOr,
        TestLabel "Check whether correct stream amount is created" testStreamAmount,
        TestLabel "Unary Operation Logical not Test" testUnaryOperationLogicalNot,
        TestLabel "Unary Operation Negate Test" testUnaryOperationNegate,
        TestLabel "Always Expression Test" testAlwaysExpression,
        TestLabel "Eventually Expression Test" testEventuallyExpression
    ]