{- 
  File      :  Markov.hs 
  Copyright : (c) Hanwen Xu, 02/20/19 
  Contains  create a Markov Model. 
-}
module Markov (
    MarkovModel,
    Markov.create,
    logProbability,
)
where
import Hashtable as HT
import Data.Vector as DV
import Data.Set as DS
data MarkovModel = MarkovModel { 
    mK      :: Int, -- k of the MarkovModel specified by user
    mCounts :: HT.Hashtable String Double, -- hashtable created according to the input String and k
    mAlpha  :: Double -- The number of unique characters 
    }
    deriving (Show)
{-
    Complexity: It creats a string from startIdx to endIdx, so O(endIdx - startIdx) = O(k)
    It is a helper function to generate a string of length k.
-}
kString :: Int -> Int -> String -> String
kString startIdx endIdx knownText
    | endIdx > Prelude.length knownText 
        = kString startIdx (Prelude.length knownText) knownText Prelude.++ (kString 0 (endIdx - (Prelude.length knownText)) knownText)
    | endIdx == startIdx = ""
    | otherwise = knownText !! startIdx : kString (startIdx + 1) endIdx knownText
{-
    Complexity: O(n^2*k). It depends on HT.lookup(O(nk)) and HT.update(O(n^2*k)).
    It is a helper function for createHT, which updates the hashtable inside the Markov Model.
    When the key doesn't exist in the hashtable, insert the value 1.
    If the key exists, increase the value by 1.
-}
updateMK :: (Eq v, Num v) => String -> Hashtable String v -> Hashtable String v
updateMK k htable  
    | pair == HT.defVal htable = HT.update (k, 1) htable
    | otherwise = HT.update (k, 1 + (snd pair)) htable
    where pair = HT.lookup k htable
{-
    Complexity: O(n^3*k), It depends on updateMK and the length of the Vector (String, String) created by createKTuples, which is n.
    It creates the hashtable for the Markov Model.
-}
createHT :: (Eq v, Num v) => Int -> String -> Hashtable String v -> Hashtable String v
createHT k knownText htable = DV.foldl (\x y -> updateMK (snd y) (updateMK (fst y) x) ) htable (createKTuples k knownText)
{-
    Complexity: O(n^3*k), it depends on createHT.
    It creates the Markov Model.
-}
create :: Int -> String -> MarkovModel
create k knownText = 
    MarkovModel {
                mK = k,
                mCounts = htable,
                mAlpha = fromIntegral $ DS.size (DS.fromList knownText) -- O(nlgn)
    }
    where 
        htable = createHT k knownText (HT.create 1000 ("", 0) 2 0.8) -- I set the initial size of the hashtable to be 1000, and load factor to br 0.8.

{-
    Complexity: O(nk)
    It creats n tuples with length k for the first string and (k+1) for the second string.
    It serves for the input of hashtable.
-}
createKTuples :: Int -> String -> Vector (String, String)
createKTuples k knownText = DV.generate (Prelude.length knownText) (\x -> createTuples x k knownText)
createTuples :: Int -> Int -> String -> (String, String)
createTuples startIdx k knownText = (short, long)
    where
        endIdx = startIdx + k
        short = kString startIdx endIdx knownText
        long = if (endIdx > Prelude.length knownText - 1)
                then short Prelude.++ [knownText !! (endIdx - Prelude.length knownText)]
                else short Prelude.++ [knownText !! endIdx] 
{-
    Complexity: O(nk), depends on HT.lookup
    It serves for calculate the logProbability.
-}
calculate :: (String, String) -> MarkovModel -> Double
calculate tuple@(s1, s2) model = Prelude.log ((m + 1) / (n +  (mAlpha model)))
    where 
        htable = mCounts model
        n = snd (HT.lookup s1 htable)
        m = snd (HT.lookup s2 htable)
{-
    Complexity: O(mnk), where m is the length of the unknown text, n is the length of the string to create the Markov Model
    It calculates the logprobability at each position.
-}
logProbability :: String -> MarkovModel -> Double
logProbability unknownText model = DV.foldl (\x y -> calculate y model + x) 0 (createKTuples (mK model) unknownText)
