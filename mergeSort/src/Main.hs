module Main where
import Control.Monad.Writer
import Control.Monad.Reader


merge [] xs = xs
merge xs [] = xs
merge (x:xs) (y:ys)
  | x<=y = x:merge xs (y:ys)
  | otherwise = y:merge (x:xs) ys

indent n = showString (take (4*n) (repeat ' '))

nl = showChar '\n'

mergesort :: Int -> [Int] -> Writer String [Int]
mergesort l [] = do
  return []

mergesort l s@[x] = do
  return [x]

mergesort l s@xs = do
  tell $ (indent l.showString "mergesort:   ".shows s.showString "\n") ""
  let (a1,a2) = splitAt (length s `div` 2) xs
  curMergeResult <- liftM2 merge (mergesort (l+2) a1) (mergesort (l+2) a2)
  tell $ (indent l.showString "after merge: " .shows curMergeResult) "\n"
  return curMergeResult
  

main :: IO ()
main = do
  let res = runWriter $ mergesort 0 [5,4,3,10,9,101,2,1]
  putStrLn $show $ fst $res
  putStrLn $ snd $ res
