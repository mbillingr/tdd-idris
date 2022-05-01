import Control.Monad.State

increase : Nat -> State Nat ()
increase inc = do current <- get
                  put (current + inc)

-- try it with (node the reversed order of arguments, compared to the book):  
--   runState 89 (increase 5)