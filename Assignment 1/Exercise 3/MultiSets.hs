module MultiSets(
    MSet(MS),
    empty,
    add,
    occs,
    elems,
    subeq,
    union,
    mapMSet
) where

newtype MSet a = MS [(a, Int)]
    deriving (Show)

{-
Constructor for an empty MSet;
-}
empty :: MSet a
empty = MS []

instance Foldable MSet where
    foldr :: (a -> b -> b) -> b -> MSet a -> b
    foldr f z (MS[]) = z
    foldr f z (MS((x, y):xs)) = f x (foldr f z (MS xs))

{-
The Eq instance for MSet is defined by applying the subeq function to the two multisets two times.
First subeq checks that all the elements of the first multiset are in the second multiset.
Then the second subeq checks that all the elements of the second multiset are in the first multiset.
If both of the conditions are true, then the two multisets are equal.
-}
instance Eq a => Eq (MSet a) where
    (==) :: Eq a => MSet a -> MSet a -> Bool
    (==) mySet1 mySet2 = subeq mySet1 mySet2 && subeq mySet2 mySet1

{-
Function that given a multiset and an element "a" returns the multiset obtained by adding the element "a" to the multiset;
-}
add :: Eq a => MSet a -> a -> MSet a
add (MS []) y = MS [(y, 1)]
add (MS ((x, y):xs)) z = if x == z then MS ((x, y+1):xs) else let (MS rest) = add (MS xs) z in MS ((x, y) : rest)

{-
Function that given a multiset and an element "a" returns the number of occurrences of "a" in the multiset;
-}
occs :: Eq a => MSet a -> a -> Int
occs (MS []) _ = 0
occs (MS ((x,y):xs)) z = if x == z then y else occs (MS xs) z

{-
Function that given a multiset returns the list of all the elements in the multiset;
-}
elems :: MSet a -> [a]
elems (MS []) = []
elems (MS ((x,y):xs)) = x : elems (MS xs)

{- 
Function that given a multiset returns the list of all the elements in the multiset with their occurrences;
-}
elemsOccs :: MSet a -> [(a, Int)]
elemsOccs (MS []) = []
elemsOccs (MS ((x,y):xs)) = (x, y) : elemsOccs (MS xs)

{-
Function that given two multisets returns True if the first multiset is a subset of the second multiset;
-}
subeq :: Eq a => MSet a -> MSet a -> Bool
subeq (MS []) set = True
subeq (MS ((x,y):xs)) set = (occs set x >= y) && subeq (MS xs) set

{-
Function that given a multiset, an element and its multiplicity returns the multiset obtained by adding the element to the multiset;
-}
addMultiple :: Eq a => MSet a -> a -> Int -> MSet a
addMultiple (MS []) x y = MS [(x, y)]
addMultiple (MS((x,y) : xs)) k v = if x == k then MS ((x, y + v) : xs) else MS ((x, y) : elemsOccs (addMultiple (MS xs) k v))

{-
Function that given two multisets returns the multiset obtained by the union of the two multisets;
-}
union :: Eq a => MSet a -> MSet a -> MSet a
union (MS[]) mset = mset
union (MS((x, y):xs)) mset = union (MS xs) (addMultiple mset x y)

{-
Function that given a function and a multiset returns the multiset obtained by applying the function to all the elements of the multiset;
-}
mapMSet :: (a -> b) -> MSet a -> MSet b
mapMSet f (MS []) = MS []
mapMSet f (MS ((x,y):xs)) = MS ((f x,y) : elemsOccs (mapMSet f (MS xs)))