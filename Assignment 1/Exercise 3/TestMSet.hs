import MultiSets
import Data.Char (toLower)
import Data.List (sort)
import System.IO

{-
Function that given a list of strings, returns the list of ciao strings ("characters in alphabetical order");
-}
ciao :: [String] -> [String]
ciao = map sort 

{-
Function that given a list of words and an empty MSet, adds the words to the MSet;
-}
addWords :: [[Char]] -> MSet [Char] -> MSet [Char]
addWords words (MS mset) =
    foldr (flip MultiSets.add) (MS mset) words

{- 
Function that given a String representing the path to the file, 
reads the file and returns the MSet of the words in the file with their multiplicity;
-}
readMSet :: String -> IO(MSet [Char])
readMSet pathfile = do
    text <- readFile pathfile
    let ciao_words = ciao (words text)
    let empty_mset = empty
    let full_mset = addWords ciao_words empty_mset
    return full_mset

{- 
Function that given a MSet and a path to the output file, writes the MSet to a file;
-}
writeMSet :: Show a => MSet a -> [Char] -> IO ()
writeMSet (MS xs) fileName = do
    let formatted = unlines $ map (\(v, n) -> show v ++ " - " ++ show n) xs
    writeFile fileName formatted

{- 
Main function to run tests
-}
main :: IO ()
main = do
    -- Read multisets from files
    m1 <- readMSet "./anagram.txt"
    m2 <- readMSet "./anagram-s1.txt"
    m3 <- readMSet "./anagram-s2.txt"
    m4 <- readMSet "./margana2.txt"

    -- Check if m1 and m4 are equal
    if m1 == m4
        then putStrLn "Multisets m1 and m4 are equal"
        else if elems m1 == elems m4 
            then putStrLn "Multisets m1 and m4 are not equal, but they have the same elements"
            else putStrLn "Multisets m1 and m4 are not equal, and they have different elements"

    -- Check if m1 is equal to the union of m2 and m3
    if m1 == union m2 m3
        then putStrLn "Multiset m1 is equal to the union of m2 and m3"
        else putStrLn "Multiset m1 is not equal to the union of m2 and m3"

    -- Write multisets m1 and m4 to output files
    writeMSet m1 "anag-out.txt"
    writeMSet m4 "gana-out.txt"
