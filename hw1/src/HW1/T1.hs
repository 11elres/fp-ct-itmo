{-# LANGUAGE LambdaCase #-}

module HW1.T1
  ( Day (..)
  , afterDays
  , daysToParty
  , isWeekend
  , nextDay
  ) where

import Numeric.Natural (Natural)

-- | Day of the week.
data Day
  = Monday     -- ^ Monday
  | Tuesday    -- ^ Tuesday
  | Wednesday  -- ^ Wednesday
  | Thursday   -- ^ Thursday
  | Friday     -- ^ Friday
  | Saturday   -- ^ Saturday
  | Sunday     -- ^ Sunday
  deriving Show

-- | Returns the day that follows the day of the week given as input.
nextDay :: Day -> Day
nextDay = \case
  Monday    -> Tuesday
  Tuesday   -> Wednesday
  Wednesday -> Thursday
  Thursday  -> Friday
  Friday    -> Saturday
  Saturday  -> Sunday
  Sunday    -> Monday

-- | Returns the day of the week after a given number of days has passed.
afterDays :: Natural -> Day -> Day
afterDays 0 day = day
afterDays n day = afterDays (n - 1) $ nextDay day

-- | Checks if the day is on the weekend.
isWeekend :: Day -> Bool
isWeekend Saturday = True
isWeekend Sunday   = True
isWeekend _        = False

-- | Computes the number of days until Friday.
daysToParty :: Day -> Natural 
daysToParty Friday = 0
daysToParty day    = (+ 1) $ daysToParty $ nextDay day
