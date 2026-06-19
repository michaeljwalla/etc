--real impl
module Stack ( StkType, push, pop, top, empty ) where

----------- data definition
--data StkType a = EmptyStk | Stk a (StkType a) --repeat Stk til EmptyStk
--push :: a -> StkType a -> StkType a
--push x s = Stk x s

--pop :: StkType a -> StkType a
--pop (Stk _ s) = s 

--top :: StkType a -> a
--top (Stk x _) = x

--empty :: StkType a
--empty = EmptyStk

---------- wrapper (newtype) definition utilizing the List
newtype StkType a = Stk [a]
push x (Stk xs) = Stk (x:xs)
pop (Stk (_:xs))= Stk xs
top (Stk (x:_)) = x
empty           = Stk []
