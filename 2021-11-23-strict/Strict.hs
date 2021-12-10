module Main where
import System.Random

main :: IO ()
main = do
  let (min_val, max_val) = computeValues 12345
  print min_val
  print max_val

computeValues :: Int -> (Int, Int)
computeValues seed = foldl go (maxBound, minBound) (take 100000 $ randoms std_gen)
  where
    std_gen = mkStdGen seed

    go :: (Int, Int) -> Int -> (Int, Int)
    go (old_min, old_max) new_val = (old_min `min` new_val, old_max `max` new_val)