import Data.Monoid
import Control.Applicative
import Control.Monad
import Data.STRef
import Control.Monad.ST

data State s a = State (s -> (a, s))

runState :: State s a -> s -> (a, s)
runState (State f) = f

instance Monad (State s) where
  return a = State $ \s -> (a, s)
  ma >>= mf = State $ \s0 ->
    let (b, s1) = runState ma s0
    in runState (mf b) s1

instance Functor (State s) where
  fmap = Control.Monad.liftM

instance Applicative (State s) where
  pure = return
  (<*>) = Control.Monad.ap


type FSM s = State s s
fsm :: (ev -> s -> s) -> (ev -> FSM s)
fsm transition = \e -> State $ \s -> (s, transition e s)

type Speaker = (SpeakerState, Level)

data SpeakerState = Sleep | Work
  deriving (Show)
  
data Level = Level Int
  deriving (Show)
  
  
quieter :: Level -> Level
quieter (Level n) = Level $ max 0 (n-1)

louder :: Level -> Level
louder (Level n) = Level $ min 10 (n+1)

data User = Button | Quieter | Louder
  deriving (Show)

speaker :: User -> FSM Speaker
speaker = fsm $ trans
  where trans Button (Sleep, n) = (Work, n)
        trans Button (Work, n) = (Sleep, n)
        trans Louder (s, n) = (s, louder n)
        trans Quieter (s, n) = (s, quieter n)


-- *Main> let res = mapM speaker [Button, Louder, Quieter, Quieter, Button]
-- *Main> runState res (Sleep, Level 2)

safeSpeaker :: User -> FSM Speaker
safeSpeaker = fsm $ trans
  where trans Button (Sleep, _) = (Work, Level 0)
        trans Button (Work, _) = (Sleep, Level 0)
        trans Quieter (Work, n) = (Work, quieter n)
        trans Louder (Work, n) = (Work, louder n)
        trans _ (Sleep, n) = (Sleep, n)


-- *Main> let res = mapM safeSpeaker [Button, Louder, Quieter, Button, Louder]
-- *Main> runState res (Sleep, Level 10)

forLoop :: i -> (i -> Bool) -> (i -> i) -> (i -> s -> s) -> s -> s
forLoop i0 pred next update s0 = runST $ do
  refI <- newSTRef i0
  refS <- newSTRef s0
  iter refI refS
  readSTRef refS
  where iter refI refS = do
        i <- readSTRef refI
        s <- readSTRef refS
        when (pred i) $ do
              writeSTRef refI $ next i
              writeSTRef refS $ update i s
              iter refI refS


