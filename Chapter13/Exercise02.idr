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


data StkInput = Number Integer
              | Inspect
              | Add | Subtract | Multiply
              | Negate
              | Drop | Duplicate              

strToInput : String -> Maybe StkInput
strToInput "" = Nothing
strToInput "?" = Just Inspect
strToInput "+" = Just Add
strToInput "-" = Just Subtract
strToInput "*" = Just Multiply
strToInput "~" = Just Negate
strToInput "drop" = Just Drop
strToInput "dup" = Just Duplicate
strToInput x = if all isDigit (unpack x)
                  then Just (Number (cast x))
                  else Nothing

mutual
    inspect : {height : _} -> StackIO height
    inspect {height = (S h)} = do top <- Top
                                  PutStr ("top: " ++ show top ++ "  (" ++ show h ++ " more items on the stack)\n")
                                  stackCalc
    inspect = do PutStr "The stacrk is empty.\n"
                 stackCalc

    tryUnaryOp : {height : _} -> (Integer -> Integer) -> StackIO height
    tryUnaryOp {height = (S _)} op = do val <- Pop
                                        Push (op val)
                                        result <- Top
                                        PutStr (show result ++ "\n")
                                        stackCalc
    tryUnaryOp _ = do PutStr "Fewer than one item on the stack\n"
                      stackCalc

    tryBinOp : {height : _} -> (Integer -> Integer -> Integer) -> StackIO height
    tryBinOp {height = (S (S _))} op = do val2 <- Pop
                                          val1 <- Pop
                                          Push (op val1 val2)
                                          result <- Top
                                          PutStr (show result ++ "\n")
                                          stackCalc
    tryBinOp _ = do PutStr "Fewer than two items on the stack\n"
                    stackCalc

    tryDrop : {height : _} -> StackIO height
    tryDrop {height = (S _)} = do _ <- Pop
                                  stackCalc
    tryDrop = do PutStr "Fewer than one item on the stack\n"          
                 stackCalc

    tryDuplicate : {height : _} -> StackIO height
    tryDuplicate {height = (S _)} = do val <- Pop
                                       Push val
                                       Push val
                                       PutStr ("duplicated " ++ show val ++ "\n")
                                       stackCalc
    tryDuplicate = do PutStr "Fewer than one item on the stack\n"          
                      stackCalc

    stackCalc : {height : _} -> StackIO height
    stackCalc = do PutStr "> "
                   input <- GetStr
                   case strToInput input of
                        Nothing => do PutStr "Invalid input\n"
                                      stackCalc
                        (Just (Number x)) => do Push x
                                                stackCalc
                        (Just Inspect) => inspect
                        (Just Add) => tryBinOp (+)
                        (Just Subtract) => tryBinOp (-)
                        (Just Multiply) => tryBinOp (*)
                        (Just Negate) => tryUnaryOp (0-)
                        (Just Drop) => tryDrop
                        (Just Duplicate) => tryDuplicate

main : IO ()
main = run forever [] stackCalc
