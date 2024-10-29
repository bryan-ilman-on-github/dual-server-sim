{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DeriveFunctor #-}

-- Dual Server Simulation in Haskell.

import System.Random
import Control.Monad
import Data.List
import Data.Function
import Data.Monoid
import Control.Applicative
import Text.Printf

-- Data Types.

-- Random Seeds for Interarrival and Service Times.
data InterarrivalSeed = I_Seed1 | I_Seed2 | I_Seed3 | I_Seed4 deriving (Show, Eq, Enum)
data ServiceSeed      = S_Seed3 | S_Seed4 | S_Seed5 | S_Seed6 deriving (Show, Eq, Enum)

-- Servers.
data Server = Server1 | Server2 deriving (Show, Eq, Enum)

-- Customer Record.
data Customer = Customer
  { customerID           :: Int             -- Customer Number.
  , randomDigitInterarr  :: Int             -- Random Digit for Interarrival Time.
  , interarrivalSeed     :: InterarrivalSeed -- Interarrival Seed.
  , timeBetweenArrivals  :: Int             -- Time Between Arrivals.
  , arrivalTime          :: Int             -- Arrival Time.
  , assignedServer       :: Server          -- Assigned Server.
  , randomDigitService   :: Int             -- Random Digit for Service Time.
  , serviceSeed          :: ServiceSeed     -- Service Seed.
  , serviceTime          :: Int             -- Service Time.
  , serviceStartTime     :: Int             -- Service Start Time.
  , serviceEndTime       :: Int             -- Service End Time.
  , waitTime             :: Int             -- Wait Time.
  } deriving (Show)

-- Type Synonyms.
type Time = Int
type RandomDigit = Int

-- Distribution Entry (Polymorphic Type).
data DistributionEntry a = DistributionEntry
  { seed           :: a           -- Seed Value.
  , probability    :: Double      -- Probability.
  , cumulativeProb :: Double      -- Cumulative Probability.
  , randRange      :: (Int, Int)  -- Random Digit Range.
  } deriving (Show, Functor)

-- Interarrival Time Distribution.
interarrivalDist :: [DistributionEntry InterarrivalSeed]
interarrivalDist =
  [ DistributionEntry I_Seed1 0.25 0.25 (1,25)
  , DistributionEntry I_Seed2 0.40 0.65 (26,65)
  , DistributionEntry I_Seed3 0.20 0.85 (66,85)
  , DistributionEntry I_Seed4 0.15 1.00 (86,100)
  ]

-- Service Time Distribution.
serviceDist :: [DistributionEntry ServiceSeed]
serviceDist =
  [ DistributionEntry S_Seed3 0.35 0.35 (1,35)
  , DistributionEntry S_Seed4 0.25 0.60 (36,60)
  , DistributionEntry S_Seed5 0.20 0.80 (61,80)
  , DistributionEntry S_Seed6 0.20 1.00 (81,100)
  ]

-- Type Class for SeedTime.
class SeedTime a where
  getTimeFromSeed :: a -> Int

instance SeedTime InterarrivalSeed where
  getTimeFromSeed I_Seed1 = 1
  getTimeFromSeed I_Seed2 = 2
  getTimeFromSeed I_Seed3 = 3
  getTimeFromSeed I_Seed4 = 4

instance SeedTime ServiceSeed where
  getTimeFromSeed S_Seed3 = 3
  getTimeFromSeed S_Seed4 = 4
  getTimeFromSeed S_Seed5 = 5
  getTimeFromSeed S_Seed6 = 6

-- Find Seed from Random Digit (Pattern Matching and Guarded Equations).
findSeed :: [DistributionEntry b] -> Int -> b
findSeed dist rd = seed $ head $ filter (\entry -> rd >= fst (randRange entry) && rd <= snd (randRange entry)) dist

-- Generate Random Digits (Recursive Function).
generateRandomDigits :: Int -> IO [Int]
generateRandomDigits n = replicateM n (randomRIO (1,100))

-- Create Customers.
createCustomers :: [Int] -> [Int] -> [Customer]
createCustomers interarrRds serviceRds = map createCustomer [1..numCustomers]
  where
    numCustomers = length interarrRds
    interarrSeeds = map (findSeed interarrivalDist) interarrRds
    interarrTimes = map getTimeFromSeed interarrSeeds
    arrivalTimes = scanl (+) 0 interarrTimes
    serviceSeeds = map (findSeed serviceDist) serviceRds
    serviceTimes = map getTimeFromSeed serviceSeeds
    
    createCustomer :: Int -> Customer
    createCustomer cid =
      Customer
        { customerID = cid
        , randomDigitInterarr = interarrRds !! (cid - 1)
        , interarrivalSeed = interarrSeeds !! (cid - 1)
        , timeBetweenArrivals = interarrTimes !! (cid - 1)
        , arrivalTime = arrivalTimes !! (cid - 1)
        , assignedServer = Server1 -- To be updated later.
        , randomDigitService = serviceRds !! (cid - 1)
        , serviceSeed = serviceSeeds !! (cid - 1)
        , serviceTime = serviceTimes !! (cid - 1)
        , serviceStartTime = 0 -- To be updated later.
        , serviceEndTime = 0 -- To be updated later.
        , waitTime = 0 -- To be updated later.
        }

-- Assign Servers and Simulate Service (Recursive Function, High-Order Functions).
simulateService :: [Customer] -> [Customer]
simulateService customers = simulate customers [] (0,0)
  where
    simulate :: [Customer] -> [Customer] -> (Time, Time) -> [Customer]
    simulate [] served _ = reverse served
    simulate (c:cs) served (s1End, s2End) =
      let startTime1 = max (arrivalTime c) s1End
          startTime2 = max (arrivalTime c) s2End
          ifServer1 = startTime1 <= startTime2
          (server, startTime, newS1End, newS2End) = if ifServer1
                                                    then (Server1, startTime1, startTime1 + serviceTime c, s2End)
                                                    else (Server2, startTime2, s1End, startTime2 + serviceTime c)
          wait = startTime - arrivalTime c
          updatedCustomer = c { assignedServer = server
                               , serviceStartTime = startTime
                               , serviceEndTime = startTime + serviceTime c
                               , waitTime = wait
                               }
      in simulate cs (updatedCustomer : served) (newS1End, newS2End)

-- Compute Insights (Folds and Monoids).
data Insights = Insights
  { avgWaitTime        :: Double
  , avgTotalTime       :: Double
  , serverIdleTimes    :: [(Server, Int)]
  , avgServiceTimes    :: [(Server, Double)]
  , serverUtilizations :: [(Server, Double)]
  } deriving (Show)

computeInsights :: [Customer] -> Insights
computeInsights customers =
  Insights
    { avgWaitTime = fromIntegral totalWait / fromIntegral numCustomers
    , avgTotalTime = fromIntegral totalTime / fromIntegral numCustomers
    , serverIdleTimes = [(Server1, idle1), (Server2, idle2)]
    , avgServiceTimes = [(Server1, avgServ1), (Server2, avgServ2)]
    , serverUtilizations = [(Server1, util1), (Server2, util2)]
    }
  where
    numCustomers = length customers
    totalWait = sum $ map waitTime customers
    totalTime = maximum $ map serviceEndTime customers

    server1Customers = filter (\c -> assignedServer c == Server1) customers
    server2Customers = filter (\c -> assignedServer c == Server2) customers

    totalServiceTime1 = sum $ map serviceTime server1Customers
    totalServiceTime2 = sum $ map serviceTime server2Customers

    idle1 = computeIdleTime server1Customers
    idle2 = computeIdleTime server2Customers

    avgServ1 = if not (null server1Customers)
               then fromIntegral totalServiceTime1 / fromIntegral (length server1Customers)
               else 0
    avgServ2 = if not (null server2Customers)
               then fromIntegral totalServiceTime2 / fromIntegral (length server2Customers)
               else 0

    util1 = if totalTime > 0
            then fromIntegral totalServiceTime1 / fromIntegral totalTime
            else 0
    util2 = if totalTime > 0
            then fromIntegral totalServiceTime2 / fromIntegral totalTime
            else 0

-- Compute Idle Time.
computeIdleTime :: [Customer] -> Int
computeIdleTime customers = initialIdle + sum idleGaps
  where
    sorted = sortOn serviceStartTime customers
    initialIdle = if null sorted then 0 else serviceStartTime (head sorted)
    gaps = zip sorted (tail sorted)
    idleGaps = [serviceStartTime c2 - serviceEndTime c1 | (c1, c2) <- gaps, serviceStartTime c2 > serviceEndTime c1]

-- Print Table Header with Fixed Widths.
printTableHeader :: IO ()
printTableHeader = printf "%-10s %-10s %-15s %-10s %-10s %-12s %-12s %-10s %-10s %-8s\n"
                      "Customer#" "Inter Seed" "Interarrival" "Arrival" "Server#" "Service Seed" "ServiceTime" "Start" "End" "Wait"

-- Print Customer Data with Fixed Widths (Using Text.Printf).
printCustomerData :: Customer -> IO ()
printCustomerData c = printf "%-10d %-10d %-15d %-10d %-10s %-12d %-12d %-10d %-10d %-8d\n"
                                (customerID c)
                                (randomDigitInterarr c)
                                (timeBetweenArrivals c)
                                (arrivalTime c)
                                (show $ assignedServer c)
                                (randomDigitService c)
                                (serviceTime c)
                                (serviceStartTime c)
                                (serviceEndTime c)
                                (waitTime c)

-- Convert Customer Data to Strings.
customerDataToStrings :: Customer -> [String]
customerDataToStrings c =
  [ show (customerID c)
  , show (randomDigitInterarr c)
  , show (timeBetweenArrivals c)
  , show (arrivalTime c)
  , show (assignedServer c)
  , show (randomDigitService c)
  , show (serviceTime c)
  , show (serviceStartTime c)
  , show (serviceEndTime c)
  , show (waitTime c)
  ]

-- Main Function.
main :: IO ()
main = do
  let numCustomers = 40                                -- Number of Customers.
  interarrRds <- generateRandomDigits numCustomers      -- Random Digits for Interarrival Times.
  serviceRds  <- generateRandomDigits numCustomers      -- Random Digits for Service Times.
  let customersInitial    = createCustomers interarrRds serviceRds  -- Create Customers.
      customersWithArrival = customersInitial                        -- Arrival times already set in createCustomers.
      customersServed      = simulateService customersWithArrival    -- Simulate Service.
      insights             = computeInsights customersServed        -- Compute Insights.
  printTableHeader                                                  -- Print Table Header.
  mapM_ printCustomerData customersServed                           -- Print Customer Data.
  putStrLn "\n--- Insights ---"
  putStrLn $ "Average Wait Time: " ++ show (avgWaitTime insights)
  putStrLn $ "Average Total Time: " ++ show (avgTotalTime insights)
  putStrLn $ "Server1 Idle Time: " ++ show (idleTime1 insights)
  putStrLn $ "Server2 Idle Time: " ++ show (idleTime2 insights)
  putStrLn $ "Server1 Utilization: " ++ printf "%.2f%%" (util1 insights * 100)
  putStrLn $ "Server2 Utilization: " ++ printf "%.2f%%" (util2 insights * 100)
  where
    idleTime1 insights = snd $ head $ serverIdleTimes insights
    idleTime2 insights = snd $ last $ serverIdleTimes insights
    util1 insights = snd $ head $ serverUtilizations insights
    util2 insights = snd $ last $ serverUtilizations insights
