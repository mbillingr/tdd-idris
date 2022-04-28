
labelFrom : Integer -> List a -> List (Integer, a)
labelFrom lbl [] = []
labelFrom lbl (x :: xs) = (lbl, x) :: labelFrom (lbl + 1) xs

label : List a -> List (Integer, a)
label = labelFrom 0
