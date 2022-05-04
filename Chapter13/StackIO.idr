import Data.Fuel
import Data.Vect

data StackCmd : Type -> Nat -> Nat -> Type where
     Push : Integer -> StackCmd () height (S height)
     Pop : StackCmd Integer (S height) height
     Top : StackCmd Integer (S height) (S height)

     GetStr : StackCmd String height height
     PutStr : String -> StackCmd () height height

     Pure : ty -> StackCmd ty height height
     (>>=) : StackCmd a height1 height2 -> (a -> StackCmd b height2 height3) -> StackCmd b height1 height3

(>>) : StackCmd () height1 height2 -> StackCmd b height2 height3 -> StackCmd b height1 height3
ma >> mb = ma >>= \_ => mb


runStack : (stk : Vect inHeight Integer) ->
           StackCmd ty inHeight outHeight ->
           IO (ty, Vect outHeight Integer)
runStack stk (Push x) = pure ((), x :: stk)
runStack (x :: xs) Pop = pure (x, xs)
runStack (x :: xs) Top = pure (x, x :: xs)
runStack stk GetStr = do x <- getLine
                         pure (x, stk)
runStack stk (PutStr x) = do putStr x
                             pure ((), stk)
runStack stk (Pure x) = pure (x, stk)
runStack stk (x >>= f) = do (x', newStk) <- runStack stk x
                            runStack newStk (f x')


data StackIO : Nat -> Type where
     Do : StackCmd a height1 height2 -> (a -> Inf (StackIO height2)) -> StackIO height1

namespace StackDo
    export
    (>>=) : StackCmd a height1 height2 -> (a -> Inf (StackIO height2)) -> StackIO height1
    (>>=) = Do

    export
    %tcinline
    (>>) : StackCmd () height1 height2 -> Inf (StackIO height2) -> StackIO height1
    ma >> mb = Do ma (\ _ => mb)


run : Fuel -> Vect height Integer -> StackIO height -> IO ()
run (More fuel) stk (Do c f) = do (res, newStk) <- runStack stk c
                                  run fuel newStk (f res)
run Dry stk p = pure ()


doAdd : StackCmd () (S (S height)) (S height)
doAdd = do val1 <- Pop
           val2 <- Pop
           Push (val1 + val2)


data StkInput = Number Integer
              | Add

strToInput : String -> Maybe StkInput
strToInput "" = Nothing
strToInput "+" = Just Add
strToInput x = if all isDigit (unpack x)
                  then Just (Number (cast x))
                  else Nothing

mutual
    tryAdd : {height : _} -> StackIO height
    tryAdd {height = (S (S h))} = do doAdd
                                     result <- Top
                                     PutStr (show result ++ "\n")
                                     stackCalc
    tryAdd = do PutStr "Fewer than two items on the stack\n"
                stackCalc

    stackCalc : {height : _} -> StackIO height
    stackCalc = do PutStr "> "
                   input <- GetStr
                   case strToInput input of
                        Nothing => do PutStr "Invalid input\n"
                                      stackCalc
                        (Just (Number x)) => do Push x
                                                stackCalc
                        (Just Add) => tryAdd

main : IO ()
main = run forever [] stackCalc
