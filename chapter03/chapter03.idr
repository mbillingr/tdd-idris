
xor : Bool -> Bool -> Bool
xor False y = y
xor True y = not y


isEven : Nat -> Bool
isEven Z = True
isEven (S k) = not (isEven k)
