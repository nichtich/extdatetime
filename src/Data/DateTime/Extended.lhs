> module Data.DateTime.Extended where

Printf is used to print values with given precision.

> import Text.Printf

You should also have a look at the existing Time library:
http://semantic.org/TimeLib/

Data.DateTime provides some helpful methods to handle
Data.Time values. We may need this later. 

import Data.DateTime

ISO 8601 is based on four basic concepts: time-points,
time-intervals, recurring time-intervals, and durations.
The concepts build on each other and/or on time-units
by defined rules. The full specification can be found at
http://xml.coverpages.org/ISO-FDIS-8601.pdf

Extended Date/Times propose extensions to ISO 8601, such 
as intervals (already in ISO 8601 but not included in many
implementations), approximate dates, questionable dates,
sets of dates and choices of dates.


Time units. We simply start with dates only and Integer
values (this should be made more precise later).

> type Second = Integer
> type Minute = Integer
> type Hour = Integer
> type Day = Integer
> type Week = Integer
> type Month = Integer
> type Year = Integer

Time-point: instant in the laps of time regarded as dimensionless

> data TimePoint = Y Year
>                | YM Year Month
>                | YMD Year Month Day
>
> instance Show TimePoint where
>       show (Y y) = printf "%04d" (y :: Integer)
>       show (YM y m) = printf "%04d-%02d" (y :: Integer) (m :: Integer)
>       show (YMD y m d) = printf "%04d-%02d-%02d" (y :: Integer) (m :: Integer) (d :: Integer)

time points must have the same precision to be equal

> instance Eq TimePoint where
>   (Y y1)         == (Y y2)         =  y1 == y2
>   (YM y1 m1)     == (YM y2 m2)     =  y1 == y2 && m1 == m2
>   (YMD y1 m1 d1) == (YMD y2 m2 d2) =  y1 == y2 && m1 == m2 && d1 == d2
>   a == b =  False

time points can be compared

> instance Ord TimePoint where
>   (Y y1)         <= (Y y2)         =  y1 <= y2
>   (YM y1 m1)     <= (YM y2 m2)     =  y1 < y2 || (y1 == y2 && m1 <= m2)
>   (YMD y1 m1 d1) <= (YMD y2 m2 d2) =  y1 < y2 || (y1 == y2 && (m1 < m2 || (m1 == m2 && d1 <= d2 ) ) )
> -- TODO: implement mixed cases


> data Duration =
>     PY Year
>   | PYM Year Month
>   | PYMD Year Month Day
>   | PW Week
> instance Show Duration where
>      show (PY y) = show y ++ " years long"
>      show (PYM y m) = show y ++ " years and " ++ show m ++ " month long"
>      show (PYMD y m d) = show y ++ " years, " ++ show m ++ " month, and " ++ show d ++ " long"
>      show (PW w) = show w ++ " weeks long"

A time-interval is defined as a portion of time between two time-points.
There are four types of time-intervals.

> data TimeInterval =
>     FromUntil     TimePoint TimePoint    -- a start and an end
>   | During        Duration               -- a duration not associated with any start or end
>   | FromFor       TimePoint Duration     -- a start and a duration
>   | DuringUntil   Duration TimePoint     -- a duration and an end
>
> instance Show TimeInterval where
>       show (FromUntil s e) = "from " ++ show s ++ " until " ++ show e
>       show (During d) = "during " ++ show d
>       show (FromFor s d) = "from " ++ show s ++ " for " ++ show d
>       show (DuringUntil d e) = show d ++ " until " ++ show e


> elapse :: TimePoint -> Duration -> TimePoint
> elapse (Y y) (PY py) = Y (y + py)
> elapse (Y y) (PYM py pm) = YM (y + py) (pm)
> elapse (YM y m) (PY py) = YM (y + py) (m)
> elapse (YM y m) (PYM py pm) = YM (y + py) (m + pm) -- TODO: flip years if m+pm > 12
> -- more combinations, probably to be defined in a better way

Check whether a given TimePoint is during an interval

> during :: TimePoint -> TimeInterval -> Bool
> during a (During d) = undefined -- time point during a floating duration


A time-collection is a set of time-points and/or time-intervals. It must not
contain other time-collections.

Time-choice: One of
- a time-collection, that one unknown element is selected from
- a time-interval, that one unknown time-interval or time-point is selected from

Questinable
 data Questionable = Questionable TimePoint | Questionable Duration
 instance Show Questionable where
       show (Questionable q) = "(" ++ show q ++ ")?"
