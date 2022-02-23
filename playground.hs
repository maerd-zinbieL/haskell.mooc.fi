-- increment :: Num a => p -> a
-- increment x = let x = x + 1 in x

-- increment' :: Num a => p -> a
-- increment' x = x where x = x + 1

-- step :: Integer -> Integer
-- step n | even n = n `div` 2
--        | otherwise = 3 *n +1

-- -- collatz x computes how many steps it takes for the Collatz sequence
-- -- to reach 1 when starting from x
-- collatz :: Integer -> Integer
-- collatz 1 = 0
-- collatz x = 1 + collatz (step x)

-- -- longest finds the number with the longest Collatz sequence for initial values
-- -- between 0 and upperBound
-- longest :: Integer -> Integer
-- longest upperBound = longest' 0 0 upperBound

-- -- helper function for longest
-- longest' :: Integer -> Integer -> Integer -> Integer
-- -- end of recursion, return longest length found
-- longest' number _ 0 = number
-- -- recursion step: check if n has a longer Collatz sequence than the current known longest
-- longest' number maxlength n = if length > maxlength
--                then longest' n length (n-1)
--                else longest' number maxlength (n-1)
--   where length = collatz n

-- class Size a where
--     size :: a -> Int

-- data MyType = MyType 
--   deriving Size

-- module Gold where

-- The golden ratio
-- phi :: Double
-- phi = (sqrt 5 + 1) / 2

-- polynomial :: Double -> Double
-- polynomial x = x^2 - x - 1

-- f x = polynomial (polynomial x)

-- main = do
--   print (polynomial phi)
--   print (f phi)

------------------------------------------------------------------------
--http://blog.sigfpe.com/2006/08/you-could-have-invented-monads-and.html
-- f :: Float -> Float
-- f x = x + 1.0

-- g :: Float -> Float
-- g x = x + 2.0


-- f' :: Float -> (Float, String)
-- f' x = (x + 1.0, "f' is called; ")

-- g' :: Float -> (Float, String)
-- g' x = (x + 2.0, "g' is called; ")

-- bind :: (Float -> (Float, String)) -> (Float, String) -> (Float, String)
-- bind f (gx, gs) = let (fx, fs) = f gx
--                   in (fx, gs++fs)

-- unit :: Float -> (Float, String)
-- unit x = (x, "")

-- lift f = unit . f

-- bind' :: (Complex Double -> [Complex Double]) -> [Complex Double] -> [Complex Double]
-- bind' f gxs = concatMap f gxs

-- unit' x = [x]

----------------------------------------------------------
-- haskell.mooc.fi
----------------------------------------------------------
-- Try to login with a password.
-- `Just username` on success, `Nothing` otherwise.
-- import           Data.List
-- import qualified Data.Map                      as Map

-- login :: String -> Maybe String
-- login "f4bulous!" = Just "unicorn73"
-- login "swordfish" = Just "megahacker"
-- login _           = Nothing

-- -- Get a secret associated with a user.
-- -- Not all users have secrets.
-- secret :: String -> Maybe String
-- secret "megahacker" = Just "I like roses"
-- secret _            = Nothing

-- -- Login and return the user's secret, if any
-- stealSecret :: String -> Maybe String
-- stealSecret password = case login password of
--   Nothing   -> Nothing
--   Just user -> case secret user of
--     Nothing -> Nothing
--     Just s  -> Just ("Stole secret: " ++ s)

-- -- Set the value of key to val in the given key-value list,
-- -- but only if val is larger than the current value!
-- increase :: Eq a => a -> Int -> [(a, Int)] -> Maybe [(a, Int)]
-- increase key val assocs = case lookup key assocs of
--   Nothing -> Nothing
--   Just x ->
--     if val < x then Nothing else Just ((key, val) : delete (key, x) assocs)

-- (?>) :: Maybe a -> (a -> Maybe b) -> Maybe b
-- Nothing  ?> cont = Nothing
-- (Just x) ?> cont = cont x

-- stealSecret' :: String -> Maybe String
-- stealSecret' password =
--   login password ?> secret ?> (\s -> Just ("Stole secret: " ++ s))

-- increase' :: Eq a => a -> Int -> [(a, Int)] -> Maybe [(a, Int)]
-- increase' key val assocs = lookup key assocs ?> check ?> update
--  where
--   check x | val < x   = Nothing
--           | otherwise = Just x
--   update x = Just ((key, val) : delete (key, x) assocs)

-- -- Logger definition
-- data Logger a = Logger [String] a
--   deriving Show

-- getVal :: Logger a -> a
-- getVal (Logger _ a) = a
-- getLog :: Logger a -> [String]
-- getLog (Logger s _) = s

-- -- Primitive operations:
-- nomsg :: a -> Logger a
-- nomsg x = Logger [] x        -- a value, no message

-- annotate :: String -> a -> Logger a
-- annotate s x = Logger [s] x  -- a value and a message

-- msg :: String -> Logger ()
-- msg s = Logger [s] ()        -- just a message

-- validateUser :: String -> Logger Bool
-- validateUser "paul.atreides" = annotate "Valid user" True
-- validateUser "ninja"         = nomsg True
-- validateUser u               = annotate ("Invalid user: " ++ u) False

-- checkPassword :: String -> String -> Logger Bool
-- checkPassword "paul.atreides" "muad'dib" = annotate "Password ok" True
-- checkPassword "ninja" "" = annotate "Password ok" True
-- checkPassword _ pass = annotate ("Password wrong: " ++ pass) False

-- login_ :: String -> String -> Logger Bool
-- login_ user password =
--   let validation = validateUser user
--   in  if getVal validation
--         then
--           let check = checkPassword user password
--           in  Logger (getLog validation ++ getLog check) (getVal check)
--         else validation

-- (#>) :: Logger a -> (a -> Logger b) -> Logger b
-- (Logger ss x) #> cont = let (Logger ss' x') = cont x in Logger (ss ++ ss') x'

-- login_' :: String -> String -> Logger Bool
-- login_' user password = validate #> check
--  where
--   validate = validateUser user
--   check False = Logger [] False
--   check True  = checkPassword user password

-- -- login_' "paul.atreides" "muad'dib"
-- -- login_' "paul.atreides" "arrakis"
-- -- login_' "ninja" ""
-- -- login_' "leto.atreides" "paul"

-- -- square a number and log a message about it
-- square :: Int -> Logger Int
-- square val = annotate (show val ++ "^2") (val ^ 2)

-- -- add 1 to a number and log a message about it
-- add :: Int -> Logger Int
-- add val = annotate (show val ++ "+1") (val + 1)

-- -- double a number and log a message about it
-- double :: Int -> Logger Int
-- double val = annotate (show val ++ "*2") (val * 2)

-- -- compute the expression 2*(x^2+1) with logging
-- compute :: Int -> Logger Int
-- compute x = square x #> add #> double

-- data Bank = Bank (Map.Map String Int)
--   deriving Show

-- deposit :: String -> Int -> Bank -> Bank
-- deposit accountName amount (Bank accounts) =
--   Bank (Map.adjust (+ amount) accountName accounts)

-- withdraw :: String -> Int -> Bank -> (Int, Bank)
-- withdraw accountName amount (Bank accounts) =
--   let -- balance is 0 for a nonexistant account
--       balance     = Map.findWithDefault 0 accountName accounts
--       -- can't withdraw over balance
--       withdrawal  = min amount balance
--       newAccounts = Map.adjust (\x -> x - withdrawal) accountName accounts
--   in  (withdrawal, Bank newAccounts)

-- share :: String -> String -> String -> Bank -> Bank
-- share from to1 to2 bank =
--   let (amount,bank1) = withdraw from 100 bank
--       half = div amount 2
--       -- carefully preserve all money, even if amount was an odd number
--       rest = amount-half
--       bank2 = deposit to1 half bank1
--       bank3 = deposit to2 rest bank2
--   in bank3

-- -- `BankOp a` is an operation that transforms a Bank value,
-- -- while returning a value of type `a`
-- data BankOp a = BankOp (Bank -> (a, Bank))

-- -- running a BankOp on a Bank
-- runBankOp :: BankOp a -> Bank -> (a, Bank)
-- runBankOp (BankOp f) bank = f bank

-- -- Running one BankOp after another
-- (+>>) :: BankOp a -> BankOp b -> BankOp b
-- op1 +>> op2 = BankOp combined
--   where combined bank = let (a, bank') = runBankOp op1 bank
--                         in runBankOp op2 bank'
-- -- f :: Bank -> (a, Bank)
-- -- g :: Bank -> (b, Bank)

-- -- Running a parameterized BankOp, using the value returned
-- -- by a previous BankOp.  The implementation is a bit tricky
-- -- but it's enough to understand how +> is used for now.
-- (+>) :: BankOp a -> (a -> BankOp b) -> BankOp b
-- op +> cont = BankOp combined
--   where combined bank = let (a, bank') = runBankOp op bank
--                         in runBankOp (cont a) bank'

-- distributeOp :: String -> String -> Int -> BankOp ()

---------------------------------------------------------------------
-- https://williamyaoh.com/posts/2020-07-12-deriving-state-monad.html
---------------------------------------------------------------------
-- Reverse a list, and increase a count of function calls
reverseWithCount :: Int -> [a] -> (Int, [a])
reverseWithCount funcCount list =
  (funcCount + 1, reverse list)

append3ReversedWithCount :: Int -> [a] -> [a] ->[a] -> (Int, [a])
append3ReversedWithCount c l1 l2 l3 = 
  let (c', rl1) = reverseWithCount c l1
      (c'', rl2) = reverseWithCount c' l2
      (c''', rl3) = reverseWithCount c'' l3
  in (c'''+1, rl1++rl2++rl3)

