
-- Exercise 1
namespace EX1
    data DoorState = DoorOpen | DoorClosed

    data DoorCmd : Type -> DoorState -> DoorState -> Type where
        Open : DoorCmd     () DoorClosed DoorOpen
        Close : DoorCmd    () DoorOpen DoorClosed
        RingBell : DoorCmd () state state

        Pure : ty -> DoorCmd ty state state
        (>>=) : DoorCmd a st1 st2 -> (a -> DoorCmd b st2 st3) -> DoorCmd b st1 st3


    (>>) : DoorCmd () st1 st2 -> Lazy (DoorCmd b st2 st3) -> DoorCmd b st1 st3
    ma >> mb = ma >>= \_ => mb


    doorProg : DoorCmd () DoorClosed DoorClosed
    doorProg = do RingBell
                  Open
                  RingBell
                  Close


-- Exercise 2
namespace EX2
    data GuessCmd : Type -> Nat -> Nat -> Type where
        Try : Integer -> GuessCmd Ordering (S guesses) guesses

        Pure : ty -> GuessCmd ty state state
        (>>=) : GuessCmd a st1 st2 -> (a -> GuessCmd b st2 st3) -> GuessCmd b st1 st3

    (>>) : GuessCmd () st1 st2 -> Inf (GuessCmd b st2 st3) -> GuessCmd b st1 st3
    ma >> mb = ma >>= \_ => mb

    threeGuesses : GuessCmd () 3 0
    threeGuesses = do _ <- Try 10
                      _ <- Try 20
                      _ <- Try 15
                      Pure ()

    --noGuesses : GuessCmd () 0 0
    --noGuesses = do _ <- Try 10; Pure ()

-- Exercise 3
namespace EX3
    data Matter = Solid | Liquid | Gas

    data MatterCmd : Type -> Matter -> Matter -> Type where
         Melt : MatterCmd     () Solid  Liquid
         Boil : MatterCmd     () Liquid Gas
         Condense : MatterCmd () Gas Liquid
         Freeze : MatterCmd   () Liquid Solid

         (>>=) : MatterCmd a st1 st2 -> (a -> MatterCmd b st2 st3) -> MatterCmd b st1 st3

    (>>) : MatterCmd () st1 st2 -> Inf (MatterCmd b st2 st3) -> MatterCmd b st1 st3
    ma >> mb = ma >>= \_ => mb

    iceSteam : MatterCmd () Solid Gas
    iceSteam = do Melt; Boil

    steamIce : MatterCmd () Gas Solid
    steamIce = do Condense; Freeze

    --overMelt : MatterCmd () Solid Liquid
    --overMelt = do Melt; Melt