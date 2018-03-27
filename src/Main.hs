
{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveAnyClass #-}
module Main where

--LTI calculator, assumes that input data format is consistent across all
--organisations

--imports
import Data.Aeson
import qualified Data.ByteString.Lazy as B
import Data.Maybe
import GHC.Generics
import System.Environment as E
--types
data Mortgage = Mortgage {
  amount :: Maybe Integer,
  income :: Income
} deriving (Eq, Ord, Show, Read)

data Income = Income [Maybe Integer] deriving (Eq, Ord, Show, Read)

data Output = Output {
  numRecords :: Int,
  amountZeroAmtLoans :: Int,
  amountNoIncomeLoans :: Int,
  thresholdLoanToIncome:: Double,
  weightedAverageLTI :: LTI,
  averageLTI :: LTI,
  loansAboveThreshold :: Int
  }deriving (Eq, Ord, Show,  Generic, ToJSON)

type Mortgages = [Mortgage]
type LTI = Double
type Loan = Maybe Integer
thresholdLTI = 5.0 :: Double
main :: IO()
main = do
  fileName <- E.getArgs
  d <- (eitherDecode <$> getJSON (head fileName)) :: IO (Either String Mortgages)
  case d of
    Left err -> putStrLn err
    Right ps -> print $ encode (computeOutput ps)
    where getJSON n = B.readFile n

--parsing from JSON
instance FromJSON Mortgage where
  parseJSON = withObject "Mortgage" $ \o -> do
    amount_ <- o .: "LoanAmt"
    firstIncome_ <- o .: "MainTotalVerifiedNetIncome"
    secondIncome_ <- o .: "SecondTotalVerifiedNetIncome"
    thirdIncome_ <- o .: "ThirdTotalVerifiedNetIncome"
    return $ Mortgage amount_ (Income [firstIncome_, secondIncome_, thirdIncome_])

getLoan :: Mortgage -> Maybe Integer
getLoan (Mortgage m _) = m

getIncome :: Mortgage -> Income
getIncome (Mortgage _ i) =  i

isZeroAmountLoan :: Loan -> Bool
isZeroAmountLoan m = (\x -> x == Nothing || x == (Just 0)) $  m
--countZeroAmt Loans
countZeroAmountLoans:: [Loan] -> Int
countZeroAmountLoans m = length [x | x <- m, isZeroAmountLoan x]

cumulativeLoanAmount :: Mortgages -> Maybe Integer
cumulativeLoanAmount m = sumMaybe [getLoan x | x<-m ]

countRecords :: Mortgages -> Int
countRecords x = length x

sumIncome :: Income  ->Maybe Integer
sumIncome  (Income i) = sumMaybe i

sumMaybe :: [Maybe Integer] -> Maybe Integer
sumMaybe x  = return $ (sum.catMaybes) x

isZeroIncome :: Income -> Bool
isZeroIncome i
            | sumIncome i <= Just 0 = True
            | otherwise = False

countZeroIncomeLoans :: [Income] -> Int
countZeroIncomeLoans m = length $ filter isZeroIncome m

mortgageLTI :: Mortgage -> Maybe LTI
mortgageLTI (Mortgage m i) = maybe_divide (fromIntegral <$> m) (fromIntegral <$> (sumIncome i))

maybe_divide ::Maybe Double -> Maybe Double -> Maybe Double
maybe_divide x y = do
   a <- x
   b <- y
   if b == 0
     then fail "Division by zero"
     else return (a/b)

cumuLativeLTI:: [Maybe LTI] -> Maybe LTI
cumuLativeLTI xs = return $ (sum.catMaybes) xs

loansOverThreshold :: [Maybe LTI] -> Int
loansOverThreshold xs = length $ filter (>= 0) (catMaybes xs)

weightedAvgLTI :: Maybe Integer -> Maybe Integer -> Maybe LTI
weightedAvgLTI l i =  maybe_divide (fromIntegral <$> l) (fromIntegral <$> i)

avgLTI:: Maybe LTI -> Int -> Maybe LTI
avgLTI c n = maybe_divide  c (return $ fromIntegral n)

computeOutput :: Mortgages -> Output
computeOutput m = let loans = map getLoan m
                      incomes  = map getIncome m
                      ltis =  map mortgageLTI m
                  in Output (countRecords m) (countZeroAmountLoans loans) (countZeroIncomeLoans incomes) (thresholdLTI) (fromJust $ weightedAvgLTI (sumMaybe loans) (sumMaybe (map sumIncome incomes))) (fromJust $ avgLTI (cumuLativeLTI ltis) (length ltis)) (loansOverThreshold ltis)
