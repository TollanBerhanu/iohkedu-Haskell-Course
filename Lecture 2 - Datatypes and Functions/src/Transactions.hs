module Transactions where

import Prelude hiding (lookup)
-- import Tables  {- Commented for the last task (Task Transactions-8) -}
import Data.Map  {- Uncommented for the last task (Task Transactions-8) -}

-- START HERE AFTER Tables.hs

-- This is the transactions datatype from the slides,
-- using record syntax.

data Transaction =
  Transaction
    { trAmount :: Amount
    , trFrom   :: Account
    , trTo     :: Account
    }
  deriving (Eq, Show)

type Amount  = Int
type Account = String

-- Both declarations below define transactions. The
-- first uses the 'Transaction' constructor normally,
-- the second uses record syntax during construction.
--
-- In the record version, we can assign the fields
-- in a different oder; otherwise, both versions are
-- equivalent.

transaction1 :: Transaction
transaction1 = Transaction 10 "Andres" "Lars"

transaction2 :: Transaction
transaction2 =
  Transaction
    { trAmount = 7
    , trFrom   = "Lars"
    , trTo     = "Alejandro"
    }

-- Task Transactions-1.
--
-- Flip a transaction, by producing a transaction of
-- the negative amount in the opposite direction.

-- |
-- >>> flipTransaction transaction1
-- Transaction {trAmount = -10, trFrom = "Lars", trTo = "Andres"}
--
flipTransaction :: Transaction -> Transaction
flipTransaction (Transaction amount from to) = Transaction (-amount) to from 

-- Task Transactions-2.
--
-- Normalize a transaction by flipping it if and only
-- if the transaction amount is negative.

normalizeTransaction :: Transaction -> Transaction
normalizeTransaction transaction
    | amount < 0 = flipTransaction transaction
    | otherwise = transaction
    where amount = (\(Transaction a _ _) -> a) transaction

-- Task Transactions-3.
--
-- Re-implement 'processTransaction' from the slides,
-- but use the function 'alter' that you defined in
-- the Tables tasks.

-- type Accounts = Table Account Amount  {- Commented for the last task (Task Transactions-8) -}
type Accounts = Map String Int   {- Uncommented for the last task (Task Transactions-8) -}

-- |
-- >>> let a = processTransaction transaction1 $ insert "Andres" 30 empty
-- >>> lookup "Andres" a
-- Just 20
-- >>> lookup "Lars" a
-- Just 10
-- >>> lookup "Alejandro" a
-- Nothing
-- >>> let b = processTransaction transaction2 a
-- >>> lookup "Lars" b
-- Just 3
-- >>> lookup "Alejandro" b
-- Just 7
--
processTransaction :: Transaction -> Accounts -> Accounts
processTransaction (Transaction amount debitAcc creditAcc) accounts 
    | (lookup debitAcc accounts == Nothing) = processTransaction transaction $ insert debitAcc 0 accounts
    | (lookup creditAcc accounts == Nothing) = processTransaction transaction $ insert creditAcc 0 accounts
    | otherwise = credit $ debit accounts
      where transaction = Transaction amount debitAcc creditAcc
            debit = alter ( >>= (\x -> Just (x-amount))) debitAcc
            credit = alter ( >>= (\x -> Just (x+amount))) creditAcc

-- Task Transactions-4.
--
-- Verify that you can no longer pattern match on the
-- 'Tables' constructor if you have hidden the 'Tables'
-- constructor from the export list as requested in the
-- Tables tasks.
{- Verified! -}

-- Task Transactions-5.
--
-- Process a list of transactions one by one.

-- |
-- >>> let a = processTransactions [transaction1, transaction2] empty
-- >>> lookup "Andres" a
-- Just (-10)
-- >>> lookup "Lars" a
-- Just 3
-- >>> lookup "Alejandro" a
-- Just 7
--
processTransactions :: [Transaction] -> Accounts -> Accounts
processTransactions [] acc = acc  
processTransactions (x:xs) acc = processTransactions xs (processTransaction x acc)

-- Task Transactions-6.
--
-- Write a version of 'processTransaction' that fails
-- if and only if the new balances would be negative.

-- |
-- >>> let Just a = processTransaction' transaction1 $ insert "Andres" 30 empty
-- >>> lookup "Andres" a
-- Just 20
-- >>> lookup "Lars" a
-- Just 10
--
-- >>> processTransaction' transaction1 empty
-- Nothing
--
processTransaction' :: Transaction -> Accounts -> Maybe Accounts
processTransaction' (Transaction amount debitAcc creditAcc) accounts 
    | debitWallet == Nothing = Nothing
    | otherwise = if (\(Just w) a -> (w - a) < 0 ) debitWallet amount
                  then Nothing 
                  else Just $ processTransaction transaction accounts
      where transaction = Transaction amount debitAcc creditAcc
            debitWallet = lookup debitAcc accounts

-- Task Transactions-7.
--
-- Write a version of 'processTransactions' that fails
-- if any of the individual transactions fail.

-- |
-- >>> let Just a = processTransactions' [transaction1, transaction2] $ insert "Andres" 50 empty
-- >>> lookup "Andres" a
-- Just 40
-- >>> lookup "Lars" a
-- Just 3
-- >>> lookup "Alejandro" a
-- Just 7
--
-- >>> processTransactions' [transaction1, transaction2] empty
-- Nothing
--
processTransactions' :: [Transaction] -> Accounts -> Maybe Accounts
processTransactions' [] acc = Just acc 
processTransactions' (x:xs) acc
  | currentTxn == Nothing = Nothing
  | otherwise = processTransactions' xs $ (\(Just x) -> x) currentTxn
  where currentTxn = processTransaction' x acc

-- Task Transactions-8.
--
-- Make your package depend on the @containers@ package.
-- For this module, switch from using the 'Tables' datatype
-- to the 'Map' datatype from the 'Data.Map' module, and
-- verify that everything still works.
{- Verified! -}

-- GO BACK to Datatypes.hs
