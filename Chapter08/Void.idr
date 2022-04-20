
twoPlusTwoNotFive : 2 + 2 = 5 -> Void
twoPlusTwoNotFive Refl impossible


loop : Void
loop = loop


valueNotSuc : (x : Nat) -> x = S x -> Void
valueNotSuc _ Refl impossible
