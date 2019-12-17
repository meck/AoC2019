module Queue (push, pop, isEmpty, empty) where

data Queue a = Queue { inbox :: [a], outbox :: [a] } deriving (Show, Eq)

push :: a -> Queue a -> Queue a
push e (Queue inb outb) = Queue (e : inb) outb

pop :: Queue a -> (Maybe a, Queue a)
pop q = case top of
    Nothing -> (top, empty)
    Just e  -> (Just e, poppedQueue)
  where
    (top, q')   = peek q
    poppedQueue = Queue (inbox q') (tail $ outbox q')

peek :: Queue a -> (Maybe a, Queue a)
peek (Queue []  []) = (Nothing, empty)
peek (Queue inb []) = peek $ Queue [] (reverse inb)
peek q              = (Just $ head (outbox q), q)

isEmpty :: Queue a -> Bool
isEmpty (Queue [] []) = True
isEmpty _             = False

empty :: Queue a
empty = Queue [] []
