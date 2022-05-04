import Data.String

VendState : Type
VendState = (Nat, Nat)

data Input = COIN | VEND | CHANGE | REFILL Nat

data MachineCmd : Type ->
                  VendState ->  -- machine state before the operation (precondition)
                  VendState ->  -- machine state after the operation (postcondition)
                  Type where
     InsertCoin : MachineCmd () (coins, chocs)      (S coins, chocs)
     Vend       : MachineCmd () (S coins, S chocs) (coins, chocs)
     GetCoins   : MachineCmd () (coins, chocs)     (Z, chocs)
     Refill     : (bars : Nat) ->
                  MachineCmd () (Z, chocs)          (Z, bars + chocs)

     Display : String -> MachineCmd () state state
     GetInput : MachineCmd (Maybe Input) state state

     Pure : ty -> MachineCmd ty state state
     (>>=) : {st2 : _} -> MachineCmd a st1 st2 -> (a -> MachineCmd b st2 st3) -> MachineCmd b st1 st3

(>>) : {st2 : _} -> MachineCmd () st1 st2 -> Inf (MachineCmd b st2 st3) -> MachineCmd b st1 st3
ma >> mb = ma >>= \_ => mb


data MachineIO : VendState -> Type where
     Do : {st1 : _} -> MachineCmd a st1 st2 -> (a -> Inf (MachineIO st2)) -> MachineIO st1

namespace MachineDo
    export
    (>>=) : {st1 : _} -> MachineCmd a st1 st2 -> (a -> Inf (MachineIO st2)) -> MachineIO st1
    (>>=) = Do

    export
    %tcinline
    (>>) : {st1 : _} -> MachineCmd () st1 st2 -> Inf (MachineIO st2) -> MachineIO st1
    ma >> mb = Do ma (\ _ => mb)


mutual
    vend : {coins : _} -> {chocs : _} -> MachineIO (coins, chocs)
    vend {coins=S p} {chocs=S c} = do Vend
                                      Display "Enjoy!"
                                      machineLoop
    vend {chocs=Z} = do Display "Out of stock"
                        machineLoop
    vend {coins=Z} = do Display "Insert a coin"
                        machineLoop

    refill : {coins : _} -> {chocs : _} -> (num : Nat) -> MachineIO (coins, chocs)
    refill {coins=Z} num = do Refill num
                              machineLoop
    refill _ = do Display "Can't refill: Coins in machine"
                  machineLoop

    machineLoop : {coins : _} -> {chocs : _} -> MachineIO (coins, chocs)
    machineLoop = do Just x <- GetInput
                        | Nothing => do Display "Invalid input"
                                        machineLoop
                     case x of
                          COIN => do InsertCoin
                                     machineLoop
                          VEND => vend
                          CHANGE => do GetCoins
                                       Display "Change returned"
                                       machineLoop
                          REFILL num => refill num
