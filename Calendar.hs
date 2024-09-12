module Calendar where
 import Prelude(Int(..),Show(..),(++),(<), String(..))

 data Date =  Date Year Month Day
 data Year = Year Int deriving(Show)
 data Day = Day Int deriving(Show)
 data Month =  JAN | FEB | MAR deriving(Show)

 instance Show Date where
  show (Date (Year y) m (Day d)) = show(y) ++ "-"++ (show m) ++ "-"++ (show d)

 data Time = Time Hour Min Sec
 data Hour = Hour Int
 data Min = Min  Int
 data Sec = Sec Int

 addZero :: Int -> String
 addZero z = if z < 10 then "0" ++ (show z) else (show z)

 instance Show Time where
  show (Time (Hour h) (Min m) (Sec s)) =  show( (Hour h))++ ":"++show((Min m)) ++ ":"++show((Sec s) )
 instance Show Hour where
  show (Hour h) = addZero h
 instance Show Min where
  show (Min h) = addZero h
 instance Show Sec where
  show (Sec h) = addZero h