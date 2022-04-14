occurences : Eq ty =>
             (item : ty) ->
             (values : List ty) ->
             Nat
occurences item [] = 0
occurences item (value :: values) =
    case item == value of
        False => occurences item values
        True => 1 + occurences item values

data Matter = Solid | Liquid | Gas

Eq Matter where
  (==) Solid Solid = True
  (==) Liquid Liquid = True
  (==) Gas Gas = True
  (==) _ _ = False

  (/=) x y = not (x == y)


data Tree x = Empty
            | Node (Tree x) x (Tree x)

Eq x => Eq (Tree x) where
  (==) Empty Empty = True
  (==) (Node left e right) (Node left' e' right')
        = left == left' && e == e' && right == right'
  (==) _ _ = False
