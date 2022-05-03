import Control.Monad.State

-- Exercise 1

update : (stateType -> stateType) -> State stateType ()
update func = do current <- get
                 put (func current)


increase : Nat -> State Nat ()
increase x = update (+x)


-- Exercise 2

data Tree a = Empty | Node (Tree a) a (Tree a)

testTree : Tree String
testTree = Node (Node (Node Empty "Jim" Empty) "Fred"
                      (Node Empty "Sheila" Empty)) "Alice"
                (Node Empty "Bob" (Node Empty "Eve" Empty))


countEmpty : Tree a -> State Nat ()
countEmpty Empty = update (+1)
countEmpty (Node left _ right) = do countEmpty left
                                    countEmpty right


-- Exercise 2

countEmptyNode : Tree a -> State (Nat, Nat) ()
countEmptyNode Empty = update (\(e,n) => (1 + e, n))
countEmptyNode (Node left _ right) = do countEmptyNode left
                                        countEmptyNode right
                                        update (\(e,n) => (e, 1 + n))
