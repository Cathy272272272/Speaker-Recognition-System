{- 
  File      :  Main.hs 
  Copyright : (c) Hanwen Xu, 02/20/19 
-}
module Main
(
    main 
)
where 

import System.IO  
import qualified Markov 
import System.Environment


usage :: String 
usage = "usage: markov  <file name for speaker A> " ++
        "<file name for speaker B>\n  <file name of text to identify> " ++
        "<order>"
{-
    It creates two models according to speakerAText and speakerBText, and uses speakerCText to test which model is more similar to speakerC.
    It compares by comparing the logProbability normailized by the length of speakerCText.
-}
identifySpeaker:: String -> String -> String -> Int -> (Double,Double,String)
identifySpeaker speakerAText speakerBText speakerCText k = (norm_a, norm_b, res) 
    where
        mk_a = Markov.create k speakerAText
        mk_b = Markov.create k speakerBText
        norm_a = ( Markov.logProbability speakerCText mk_a )  / (fromIntegral $ Prelude.length speakerCText)
        norm_b = ( Markov.logProbability speakerCText mk_b )  / (fromIntegral $ Prelude.length speakerCText)
        res = if norm_a > norm_b then "A" else "B"

printResults :: (Double,Double,String) -> IO () 
printResults (likelihoodA, likelihoodB, conclusion) = do 
    putStrLn $ "Speaker A:" ++ show likelihoodA 
    putStrLn $ "Speaker B:" ++ show likelihoodB 
    putStrLn "" 
    putStrLn $ "Conclusion: Speaker " ++ show conclusion ++ " is most likely"

main :: IO()
main = do 
    args <- getArgs
    case args of
        [aFile,bFile,cFile,kStr] -> do 
            aContents <- readFile aFile 
            bContents <- readFile bFile
            cContents <- readFile cFile 
            let k = read kStr :: Int
            let results = identifySpeaker aContents bContents cContents k 
            printResults results
        _ -> putStrLn usage 


