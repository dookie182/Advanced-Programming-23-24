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

instance Eq a => Eq (MSet a) where
    (==) :: Eq a => MSet a -> MSet a -> Bool
    (==) mySet1 mySet2 = subeq mySet1 mySet2 && subeq mySet2 mySet1

{-
Returns the multiset obtained by adding an element "a" to the multiset;
-}
add :: Eq a => MSet a -> a -> MSet a
add (MS []) y = MS [(y, 1)]
add (MS ((x, y):xs)) z = if x == z then MS ((x, y+1):xs) else let (MS rest) = add (MS xs) z in MS ((x, y) : rest)

{-
Returns the number of occurrences of an element "a" in the multiset;
-}
occs :: Eq a => MSet a -> a -> Int
occs (MS []) _ = 0
occs (MS ((x,y):xs)) z = if x == z then y else occs (MS xs) z

{-
Returns the list of all the elements in the multiset;
-}
elems :: MSet a -> [a]
elems (MS []) = []
elems (MS ((x,y):xs)) = x : elems (MS xs)

{- 
Returns the list of all the elements in the multiset with their occurrences;
-}
elemsOccs :: MSet a -> [(a, Int)]
elemsOccs (MS ((x,y):xs)) = (x, y) : elemsOccs (MS xs)

{-
Returns True if the first multiset is a subset of the second multiset;
-}
subeq :: Eq a => MSet a -> MSet a -> Bool
subeq (MS []) set = True
subeq (MS ((x,y):xs)) set = (occs set x >= y) && subeq (MS xs) set

{-
Returns the union of two multisets;
-}
union:: Eq a => MSet a -> MSet a -> MSet a
union (MS []) set = set
union set (MS []) = set
union (MS ((x,y):xs)) (MS ((z,w):zs)) = if x == z then MS ((x, y+w) : elemsOccs (MS xs `union` MS zs)) else MS ((x,y) : elemsOccs (MS xs `union` MS ((z,w):zs)))

{-
Returns the multiset obtained by applying a function f: a -> b to all the elements of the multiset;
-}
mapMSet :: (a -> b) -> MSet a -> MSet b
mapMSet f (MS []) = MS []
mapMSet f (MS ((x,y):xs)) = MS ((f x,y) : elemsOccs (mapMSet f (MS xs)))