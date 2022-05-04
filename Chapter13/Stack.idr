import Data.Vect

data StackCmd : Type -> Nat -> Nat -> Type where
     Push : Integer -> StackCmd () height (S height)
     Pop : StackCmd Integer (S height) height
     Top : StackCmd Integer (S height) (S height)

     Pure : ty -> StackCmd ty height height
     (>>=) : StackCmd a st1 st2 -> (a -> StackCmd b st2 st3) -> StackCmd b st1 st3

(>>) : StackCmd () st1 st2 -> Inf (StackCmd b st2 st3) -> StackCmd b st1 st3
ma >> mb = ma >>= \_ => mb


testAdd : StackCmd Integer 0 0
testAdd = do Push 10
             Push 20
             val1 <- Pop
             val2 <- Pop
             Pure (val1 + val2)


runStack : (stk : Vect inHeight Integer) ->
           StackCmd ty inHeight outHeight ->
           (ty, Vect outHeight Integer)
runStack stk (Push x) = ((), x :: stk)
runStack (x :: xs) Pop = (x, xs)
runStack (x :: xs) Top = (x, x :: xs)
runStack stk (Pure x) = (x, stk)
runStack stk (cmd >>= next) = let (cmdRes, newStk) = runStack stk cmd in
                                  runStack newStk (next cmdRes)

doAdd : StackCmd () (S (S height)) (S height)
doAdd = do val1 <- Pop
           val2 <- Pop
           Push (val1 + val2)