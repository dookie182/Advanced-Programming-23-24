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
Function that given a multiset, an element and a number returns the multiset obtained by adding the element to the multiset with the given number of occurrences;
-}
addMerge :: Eq a => MSet a -> a -> Int -> MSet a
addMerge (MS []) x y = MS [(x, y)]
addMerge (MS((x,y) : xs)) k v = if x == k then MS ((x, y + v) : xs) else MS ((x, y) : elemsOccs (addMerge (MS xs) k v))

{-
Function that given two multisets returns the multiset obtained by the union of the two multisets;
-}
union :: Eq a => MSet a -> MSet a -> MSet a
union (MS[]) mset = mset
union (MS((x, y):xs)) mset = union (MS xs) (addMerge mset x y)

{-

It's not possibile to define a Functor instance for MSet because applying the map function 
it's not guaranteed that the multiplicity of the elements will be preserved. 
We can think to a simple example of a function that maps each element of the multiset to a single key,
in this case the multiplicity of the elements will be lost and we are violating the MSet properties.
In fact, each element of an MSet should be present only once in the multiset with a multiplicity greater than 0 by definition.

For this reason we have to define Eq b in the signature of the function mapMSet and so fmap signature it's not respected.

Function that given a function and a multiset returns the union of the Mset and an empty MSet 
with the elements of the first multiset mapped by the function. The union operations guarantees that the multiplicity of the elements is preserved 
and keys are unique in the Mset.
-}
mapMSet ::Eq b => (a -> b) -> MSet a -> MSet b
mapMSet f (MS []) = MS []
mapMSet f (MS xs) = MS (map (\(x, n) -> (f x, n)) xs) `union` MS []

