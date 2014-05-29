module Utils (
   combinations,
   interactAll
) where


combinations :: [a] -> [(a, a)]
combinations [] = []
combinations (x:xs) = [(x,y) | y <- xs] ++ combinations xs


interactAll :: (a -> [a] -> a) -> [a] -> [a]
interactAll _ [] = []
interactAll interact (x:xs) = reverse (interactAll' [] x xs [])
   where
      interactAll' !prev !mid [   ] !res = interact mid prev : res
      interactAll' !prev !mid !rest !res = 
         let res' = interact mid (prev ++ rest) : res
         in interactAll' (mid:prev) (head rest) (tail rest) res'

