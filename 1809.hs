p a b c = (a+b+c)/2
square a b c = sqrt( p a b c * (p a b c - a) * (p a b c - b) * (p a b c - c))


square a b c = sqrt( p  * (p  - a) * (p  - b) * (p  - c))
  where p = (a + b + c)/2


square a b c = sqrt( p  * pa * pb * pc)
  where p = (a + b + c)/2
        pa = p-a
        pb = p-b
        pc = p-c


square a b c = let p = (a + b + c)/2 
               in sqrt( p  * (p  - a) * (p  - b) * (p  - c))
  
--beside :: Int -> (Int, Int)
--beside x@(succ y) = (y, succ x)

--toStr :: Int -> String
--toStr x =
--   case x of
--       0 -> "None"
--       1 -> "One"
--       2 -> "Two"
--       _ -> "!!!!!"
    
--toStr x 
--   | x < 0 = "Negative"
--   | x == 0 = "Zero"
--   | x <12 = "Several"
--   | True = "Many" 
-- | otherwise = "Many" можно и так 

--if bool 
--then 3 
--else 5


maximum' :: (Ord a ) => [a] -> a
maximum' [] = error "empty list"
maximum' [x] = x
maximum' (x: xs) = max x (maximum' xs)

tt =[1,2,3,4,5]


take' ::  Int -> [a] -> [a]
take' s a = if s>length a then  error "empty list" else if s==0 then [] else [head (a)] ++ (take' (s-1) (tail a))

reverse' ::  [a] -> [a]
reverse' a = if length a ==0 then [] else reverse' (tail a) ++  [head (a)]

replicate' :: Int -> a -> [a]
replicate' s a = if s==0 then [] else [a] ++ (replicate'  (s-1) a)

elem' :: Eq a => a -> [a] -> Bool
elem' t a = if length a == 0 then False else if (head a)== t then True else elem' t (tail a)

drop' :: Int -> [a] -> [a]
drop' s a = if s>length a then  error "empty list" else if s==0 then a else   drop (s-1) (tail a)

sum' ::  (Num a ) => [a] -> a
sum' a = if length a ==0 then 0 else (head a) + sum' (tail a)
    
null' ::  [a] -> Bool
null' [] = True
null' _ = False

null'  a = if length a == 0 then True else False

last' :: [a] -> a
last' a = if length a ==0 then error "empty list" else if length a==1 then head a else last (tail a)

init' :: [a] -> [a]
init'  a = if length a ==0 then error "empty list" else take' ((length a) -1 ) a

g x = x*x*x-15*x

parse' l r h  = if l> r then [] else [(l,g l)]++ parse' (l+h) r h

buf_fib = [1,1]


--ff:: (Num a, Ord a) =>
ff n  vec = if n >= last vec  then ff n (vec ++ [(last vec) + (last(init vec))]) else vec

fib nn = ff nn buf_fib
