import Data.List

data State : (stateType : Type) -> Type -> Type where
     Get : State stateType stateType
     Put : stateType -> State stateType ()

     Pure : ty -> State stateType ty
     Bind : State stateType a -> (a -> State stateType b) -> State stateType b

mutual
    Functor (State stateType) where
        map func x = do val <- x
                        pure (func val)

    Applicative (State stateType) where
        pure = Pure
        (<*>) f a = do f' <- f
                       a' <- a
                       pure (f' a') 

    Monad (State stateType) where
        (>>=) = Bind

runState : State stateType a -> (st : stateType) -> (a, stateType)
runState Get st = (st, st)  -- produce current state and leave state unchanged
runState (Put newState) st = ((), newState)
runState (Pure x) st = (x, st)
runState (Bind cmd prog) st = let (val, nextState) = runState cmd st in
                                  runState (prog val) nextState


addIfPositive : Integer -> State Integer Bool
addIfPositive val = do when (val > 0) $
                            do current <- Get
                               Put (current + val)
                       Pure (val > 0)

addPositives : List Integer -> State Integer Nat
addPositives vals = do added <- traverse addIfPositive vals
                       Pure (length (filter id added))
