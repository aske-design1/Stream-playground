module Monitor where

import Stream.Types as So ( Env, StreamOutState (..), Devices )

import Data.IntMap as M ( toList )
import Control.Monad (foldM_, zipWithM, when)
import Data.Sequence as S ()
import Data.Foldable (traverse_)

monitor :: Env -> [Int] -> Devices -> Bool -> IO ()
monitor (so, _) time devicesOverTime shouldPrint = foldM_ stepMonitor (toArray so) time
    where
        toArray = map snd . M.toList

        stepMonitor soStreams t = 
            putStrLn ("------------------ Time: " ++ show t ++ " -------------------------") >>
            zipWithM (\so' propNum -> do
                        -- Insert t
                        let withNewT = So.insert so' (So.verdicts so') t
                        -- Eval the streams
                        let evaluatedSo = So.update so' withNewT devicesOverTime t

                        -- Print for user
                        putStrLn $ "Property " ++ show propNum ++ ": " ++ show (So.getColVerdict so' evaluatedSo)

                        -- Extra information
                        Control.Monad.when shouldPrint $ traverse_ (\(k, v) -> putStrLn $ "t = " ++ show k ++ ", verdict = " ++ show v) (M.toList evaluatedSo)
                        
                        -- Clean up
                        return so' { So.verdicts = So.cleanUp so' evaluatedSo }
                    ) soStreams ([1..] :: [Int])
