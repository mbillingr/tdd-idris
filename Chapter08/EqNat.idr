
data EqNat : (num1 : Nat) -> (num2 : Nat) -> Type where
     Same : (num : Nat) -> EqNat num num


sameS : EqNat k j -> EqNat (S k) (S j)
sameS (Same _) = Same (S k)

checkEqNat : (num1 : Nat) -> (num2 : Nat) -> Maybe (EqNat num1 num2)
checkEqNat 0 0 = Just (Same 0)
checkEqNat 0 (S k) = Nothing
checkEqNat (S k) 0 = Nothing
checkEqNat (S k) (S j) = case checkEqNat k j of
                              Nothing => Nothing
                              (Just eq) => Just (sameS eq)
