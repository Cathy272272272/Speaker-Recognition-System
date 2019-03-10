

{- 
  File      :  Hashtable.hs 
  Copyright : (c) Hanwen Xu, 02/20/19 
  Contains  create a Hashtable. 
-}
module Hashtable (
    Hashtable,
    Hashtable.create,
    Hashtable.lookup,
    Hashtable.update,
    defVal, -- I export defVal since I use this function in Markov.hs
)
where
import Data.Vector as DV
import Data.Char as DC
data Hashtable k v = Hashtable { 
                                cells    :: DV.Vector (k, v), -- cells is a vector of key-value pairs
                                defVal   :: (k,v), -- default key-value pairs
                                growFact :: Int, -- growing factor when rehashing
                                load     :: Int, -- initial size of hash table
                                loadFact :: Double, -- upperbound of loading factor, s.t. pins <= load * loadFact
                                pins     :: Double -- number of non-default key-value pairs
                               }
                               deriving (Show)
{-
    Complexity: O(n)
    This function initializes a hashtable.
-}
create :: Int -> (k,v) -> Int -> Double -> Hashtable k v
create initSize defPair gFact lFact  = 
    Hashtable {
            -- O(n) for the DV.replicate
            cells = DV.replicate (initSize - 1) defPair,
            load = initSize,
            defVal = defPair,
            growFact = gFact,
            loadFact = lFact,
            pins = 0
    }  
{-
    Complexity: O(n^3 * k)
    This function rehashes the hashtable when pins > load * loadFact
-}

rehash :: (Show k, Eq k, Eq v) => Hashtable k v -> Hashtable k v 
rehash htable = 
    Hashtable {
        cells = cells newht,
        load = load newht,
        defVal = defVal newht,
        growFact = growFact newht,
        loadFact = loadFact newht,
        pins = pins htable
    }
    where 
        newhtable = Hashtable.create (growFact htable * (load htable)) (defVal htable) (growFact htable) (loadFact htable)
        -- O(pins * O(update)) = O(n * n^2 * k) = O(n^3 * k)
        newht = DV.foldl (\x y -> Hashtable.update y x) newhtable (cells htable)

{-
    Complexity: O(n), may loop the entire hash table
    This is a helper function of Hashtable.lookup, it loops the entire vector of the hashtable.
-}
loopAndCheck :: (Show k, Eq k, Eq v) => Int -> Hashtable k v -> k -> (k, v)
loopAndCheck idx htable k
    | idx == DV.length vec = loopAndCheck 0 htable k
    | fst pair == k = pair
    | fst pair == fst (defVal htable) = pair
    | otherwise = loopAndCheck (idx + 1) htable k
    where vec = cells htable
          pair = vec DV.! idx
{-
    Complexity: O(nk), depends on loopAndCheck, which is O(n) and hash, which is O(k)
    This function looksup the specific k.
-}
lookup :: (Show k, Eq k, Eq v) => k -> Hashtable k v -> (k,v)
lookup k htable = loopAndCheck idx htable k
    where idx = hash k (load htable)

{-
    Complexity: O(updateCells * hash) = O(n^2*k)
    This function updates the key-value pairs.
-}
update :: (Show k, Eq k, Eq v) => (k,v) -> Hashtable k v -> Hashtable k v
update pair@(k,v) htable  
    | ((pins htable) > upperbound - 1) = Hashtable.update pair (rehash htable)
    | otherwise = 
        Hashtable {
                cells = fst newcells,
                load = load htable,
                defVal = defVal htable,
                growFact = growFact htable,
                loadFact = loadFact htable,  
                pins = newpins
        }        
    where 
        newcells = updateCells pair htable (hash k (load htable))
        upperbound = (fromIntegral (load htable)) * (loadFact htable)
        newpins = if snd newcells == True then pins htable else pins htable + 1
{-
    Complexity: O(n^2), first loop the hashtable to find the correct index(O(n)), then use DV.// to update the vector of hashtable (O(n)) 
    This is a helper function of Hashtable.update.
-}
updateCells :: (Show k, Eq k, Eq v) => (k, v) -> Hashtable k v -> Int -> (DV.Vector (k, v), Bool)
updateCells pair@(k, v) htable idx 
    | idx == DV.length vec = updateCells pair htable 0
    -- O(n), n is the length of vec
    | vec DV.! idx == defVal htable = (vec DV.// [(idx, pair)], False)
    | fst (vec DV.! idx) == fst pair = (vec DV.// [(idx, pair)], True)
    | otherwise = updateCells pair htable (idx + 1) 
    where 
        vec = cells htable

{-
    Complexity: O(k)
    This is a hash function used for this hash table implementation.
-}
hash :: (Show k) => k -> Int -> Int
hash k size = Prelude.foldl (\y x -> rem (y * 37 + x) size) 0 (Prelude.map ord (show k))