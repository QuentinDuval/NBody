module UtilsTest where
import Utils



allTests :: IO()
allTests =
   let runTest (str, content) = putStrLn $ str ++ ": " ++ show content
   in mapM_ runTest [("combinations", combinationsTest),
                     ("interactAll", interactAllTest)]


combinationsTest :: Bool
combinationsTest = res1 where
   res1 = combinations [1, 2, 3, 4] == [(1,2), (1,3), (1,4), (2,3), (2,4), (3,4)]


interactAllTest :: Bool
interactAllTest = res1 && res2 where
   res1 = interactAll (foldl (-))  [0, 1, 2] == [-3, -1, 1]
   res2 = interactAll (foldl (++)) ["a", "b", "c"] == ["abc", "bac", "cba"]

