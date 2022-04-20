
import Data.Nat

zeroNotSuc : 0 = S k -> Void
zeroNotSuc Refl impossible

sucNotZero : S k = 0 -> Void
sucNotZero Refl impossible

noRec : (k = j -> Void) -> S k = S j -> Void
noRec contra Refl = contra Refl

checkEqNat : (num1 : Nat) -> (num2 : Nat) -> Dec (num1 = num2)
checkEqNat 0 0 = Yes Refl
checkEqNat 0 (S k) = No zeroNotSuc
checkEqNat (S k) 0 = No sucNotZero
checkEqNat (S k) (S j) = case checkEqNat k j of
                              Yes prf => Yes (cong S prf)
                              No contra => No (noRec contra)