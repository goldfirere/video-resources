{-# LANGUAGE LinearTypes, GADTSyntax, RankNTypes, BlockArguments #-}

module Linear3 where

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
  Bank :: Int -> Bank       -- constructor not exported

newtype Cents t = Cents Int -- constructor not exported

data Token t = Token        -- constructor not exported

-- Linearity ensures that the resulting Int equals the input Int
withCents :: Int -> (forall t. Bank %1-> Token t %1-> Cents t -> (r, Bank)) -> (r, Int)
withCents n f = let (result, Bank ending_money) = f (Bank 0) Token (Cents n) in
                (result, ending_money)

split :: Int -> Token t %1-> Cents t
      -> (forall t1 t2. Token t1 %1-> Cents t1 -> Token t2 %1-> Cents t2 -> r) %1
      -> r
split n Token (Cents m) k
  | n > m     = k Token (Cents 0) Token (Cents m)
  | otherwise = k Token (Cents n) Token (Cents (m - n))

view :: Token t %1-> Cents t -> (Ur Int, Token t)
view Token (Cents n) = (Ur n, Token)

spend :: Token t %1-> Cents t -> Bank %1-> Bank
spend Token (Cents n) (Bank b) = Bank (b + n)

--------------------------------------------

main :: IO ()
main = do
  putStr "Enter an initial number of cents: "
  cents_string <- getLine

  cents <- case readMaybe cents_string of
    Nothing -> do putStrLn "That's not a number."
                  exitFailure
    Just c  -> return c

  let go :: Int -> Bank %1-> Token t %1-> Cents t -> (Int, Bank)
      go n bank0 tok0 money = view tok0 money & \(Ur amount, tok1) ->
                              if amount <= n
                              then (n, spend tok1 money bank0)
                              else split n tok1 money
                                   \ triangle_tok triangle_money tok2 money2 ->
                                   go (n+1) (spend triangle_tok triangle_money bank0)
                                            tok2 money2

      (num_steps, ending_money) = withCents cents (go 1)

  putStrLn ("Number of steps needed: " ++ show num_steps)
  putStrLn ("Final amount of money:  " ++ show ending_money)
