{-# LANGUAGE LinearTypes, GADTSyntax #-}

module Linear2 where

import Text.Read     ( readMaybe )
import System.Exit   ( exitFailure )

-- general utility

data Ur a where
  Ur :: a -> Ur a

(&) :: a %1-> (a %1-> b) %1-> b
x & f = f x

------------------------------------------
-- logically separate module

data Bank where
  Bank :: Int -> Bank    -- constructor not exported

data Cents where
  Cents :: Int -> Cents  -- constructor not exported

-- Linearity ensures that the resulting Int equals the input Int
-- (but not with a sneaky 'view' function)
withCents :: Int -> (Bank %1-> Cents %1-> (r, Bank)) -> (r, Int)
withCents n f = let (result, Bank ending_money) = f (Bank 0) (Cents n) in
                (result, ending_money)

split :: Int -> Cents %1-> (Cents, Cents)
split n (Cents m)
  | n > m     = (Cents 0, Cents m)
  | otherwise = (Cents n, Cents (m - n))

view :: Cents %1-> (Ur Int, Cents)
view (Cents n) = (Ur n, Cents (n-1))

spend :: Cents %1-> Bank %1-> Bank
spend (Cents n) (Bank b) = Bank (b + n)

--------------------------------------------

main :: IO ()
main = do
  putStr "Enter an initial number of cents: "
  cents_string <- getLine

  cents <- case readMaybe cents_string of
    Nothing -> do putStrLn "That's not a number."
                  exitFailure
    Just c  -> return c

  let go :: Int -> Bank %1-> Cents %1-> (Int, Bank)
      go n bank0 money0 = view money0 & \(Ur amount, money1) ->
                          if amount <= n
                          then (n, spend money1 bank0)
                          else split n money1 & \(triangle_money, money2) ->
                               go (n+1) (spend triangle_money bank0) money2

      (num_steps, ending_money) = withCents cents (go 1)

  putStrLn ("Number of steps needed: " ++ show num_steps)
  putStrLn ("Final amount of money:  " ++ show ending_money)
