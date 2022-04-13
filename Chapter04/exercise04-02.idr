import Data.Vect


-- exercise 1,2

data PowerSource = Petrol | Pedal | Electric

data Vehicle : PowerSource -> Type where
     Bicycle : Vehicle Pedal
     Unicycle : Vehicle Pedal
     Motorcycle : (fuel : Nat) -> Vehicle Petrol
     Car : (fuel : Nat) -> Vehicle Petrol
     Bus : (fuel : Nat) -> Vehicle Petrol
     Tram : (power : Nat) -> Vehicle Electric
    
wheels : Vehicle power -> Nat
wheels Bicycle = 2
wheels Unicycle = 1
wheels (Motorcycle _) = 2
wheels (Car fuel) = 4
wheels (Bus fuel) = 4
wheels (Tram power) = 8

refuel : Vehicle Petrol -> Vehicle Petrol
refuel (Motorcycle fuel) = Motorcycle 50
refuel (Car fuel) = Car 100
refuel (Bus fuel) = Bus 200


-- exercise 3, 4

vectTake : (k: Fin n) -> Vect n a -> Vect (finToNat k) a
vectTake FZ xs = []
vectTake (FS x) (y :: xs) = y :: vectTake x xs


-- exercise 5

sumEntries : Num a => {n: Nat} -> (pos : Integer) -> Vect n a -> Vect n a -> Maybe a
sumEntries pos xs ys = case integerToFin pos n of
                            Nothing => Nothing
                            (Just idx) => Just (index idx xs + index idx ys)
