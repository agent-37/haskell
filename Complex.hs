module Complex where
 import Prelude(Int(..),Show(..),(++),Eq(..),Double(..),Num(..),Float(..),Fractional(..),Bool(..),(&&),Ord(..),Ordering(..))

 data Complex = Complex Double Double

 instance Show Complex where
  show (Complex real im) = show(real) ++ " "++ (show im) ++ "i"

 instance Eq Complex where
  (==) (Complex real1 im1)  (Complex real2 im2) = if( (real1 == real2) && (im1 == im2)) then True else False

 instance Num Complex where
  (+)  (Complex real1 im1)  (Complex real2 im2) = Complex ( real1 + real2) (im1+ im2)
  (-)  (Complex real1 im1)  (Complex real2 im2) = Complex ( real1 - real2) (im1- im2)
  (*)  (Complex real1 im1)  (Complex real2 im2) = Complex ( real1 * real2- im1*im2) (im1* real2+ im2* real1)
  abs  (Complex real1 im1) = Complex (real1) (0)
  negate  (Complex real1 im1) = Complex (- real1) (- im1)
  signum  (Complex real1 im1) = Complex (0) (im1)
  fromInteger  real = Complex (fromInteger real) 0

 instance Fractional Complex where
  (/) (Complex real1 im1) (Complex real2 im2) = Complex ((real1*real2 + im1*im2 )/(real2*real2 + im2*im2)) ((im1*real2 - real1*im2 )/(real2*real2 + im2*im2))
  fromRational real = Complex (fromRational real) 0

 instance Ord Complex where
  compare   (Complex real1 im1)  (Complex real2 im2) = if real1 < real2 then  LT else if (real1 == real2) then EQ else GT
  (<) (Complex real1 im1)  (Complex real2 im2) = if real1 < real2 then True else False
  (<=) (Complex real1 im1)  (Complex real2 im2) = if( (real1 <= real2)) then True else False
  (>) (Complex real1 im1)  (Complex real2 im2) = if( (real1 > real2) ) then True else False
  (>=) (Complex real1 im1)  (Complex real2 im2) = if( (real1 >= real2) ) then True else False
  max (Complex real1 im1)  (Complex real2 im2) = if( (real1 >= real2) ) then Complex real1 im1 else Complex real2 im2
  min (Complex real1 im1)  (Complex real2 im2) = if( (real1 >= real2) ) then Complex real2 im2 else Complex real1 im1
