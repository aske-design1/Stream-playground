module StreamTests.VerdictTest where

import Stream.Verdict 
import Test.HUnit 


--- Conjunction, Disjunction
testConjunction = TestCase $
    let t = TTrue
        f = FFalse
        d = Undecided

        tValues = [t | _ <- [0..10]]
    in 
        assertEqual "Should be True" (vConjunction tValues) t >>
        assertEqual "Should be False" (vConjunction (f:tValues)) f >>
        assertEqual "Should be Undecided" (vConjunction (d:tValues)) d >>
        assertEqual "Should be False" (vConjunction (d:f:tValues)) f


testDisjunction = TestCase $ 
    let t = TTrue
        f = FFalse
        d = Undecided

        fValues = [f | _ <- [0..10]]
    in
        assertEqual "Should be False" (vDisjunction fValues) f >>
        assertEqual "Should be True" (vDisjunction (t:fValues)) t >>
        assertEqual "Should be Undecided" (vDisjunction (d:fValues)) d >>
        assertEqual "Should be True" (vDisjunction (d:t:fValues)) t

verdictTests :: Test
verdictTests = TestList 
    [
        TestLabel "Conjunction" testConjunction,
        TestLabel "Disjunction" testDisjunction
    ]