import Data.String

-- Exercise 1
namespace EX1
    data Access = LoggedOut | LoggedIn
    data PwdCheck = Correct | Incorrect

    data ShellCmd : (ty : Type) -> Access -> (ty -> Access) -> Type where
         Password : String -> ShellCmd PwdCheck LoggedOut (\chk => case chk of Correct => LoggedIn; Incorrect => LoggedOut)
         Logout : ShellCmd () LoggedIn (const LoggedOut)
         GetSecret : ShellCmd String LoggedIn (const LoggedIn)

         PutStr : String -> ShellCmd () state (const state)
         Pure : (res : ty) -> ShellCmd ty (state_fn res) state_fn
         (>>=) : ShellCmd a st1 st2_fn ->
                 ((res : a) -> ShellCmd b (st2_fn res) st3_fn) ->
                 ShellCmd b st1 st3_fn

    (>>) : ShellCmd () st1 st2_fn -> Inf (ShellCmd b (st2_fn ()) st3_fn) -> ShellCmd b st1 st3_fn
    ma >> mb = ma >>= \() => mb

    session : ShellCmd () LoggedOut (const LoggedOut)
    session = do Correct <- Password "wurzel"
                    | Incorrect => PutStr "Wrong password"
                 msg <- GetSecret
                 PutStr ("Secret code: " ++ show msg ++ "\n")
                 Logout

    {-sessionBad : ShellCmd () LoggedOut (const LoggedOut)
    sessionBad = do _ <-Password "wurzel"
                    msg <- GetSecret
                    PutStr ("Secret code: " ++ show msg ++ "\n")
                    Logout-}

    {-noLogout : ShellCmd () LoggedOut (const LoggedOut)
    noLogout = do Correct <- Password "wurzel"
                      | Incorrect => PutStr "Wrong password"
                  msg <- GetSecret
                  PutStr ("Secret code: " ++ show msg ++ "\n")-}

-- Exercise 2
namespace EX2
    VendState : Type
    VendState = (Nat, Nat)

    data Input = COIN | VEND | CHANGE | REFILL Nat

    data CoinResult = Inserted | Rejected

    InsertHelper : {coins : Nat} -> {chocs : Nat} -> CoinResult -> (Nat, Nat)
    InsertHelper res = case res of
                            Inserted => (S coins, chocs)
                            Rejected => (coins, chocs)

    data MachineCmd : Type ->
                      VendState ->
                      (ty -> VendState) ->
                      Type where
        InsertCoin : MachineCmd CoinResult (coins, chocs) InsertHelper 
        Vend       : MachineCmd () (S coins, S chocs) (const (coins, chocs))
        GetCoins   : MachineCmd () (coins, chocs)     (const (Z, chocs))
        Refill     : (bars : Nat) ->
                     MachineCmd () (Z, chocs)         (const (Z, bars + chocs))

        Display : String -> MachineCmd () state (const state)
        GetInput : MachineCmd (Maybe Input) state (const state)

        Pure : (res : ty) -> MachineCmd ty (state_fn res) state_fn
        (>>=) : MachineCmd a st1 st2_fn ->
                 ((res : a) -> MachineCmd b (st2_fn res) st3_fn) ->
                 MachineCmd b st1 st3_fn

    (>>) : MachineCmd () st1 st2_fn -> Inf (MachineCmd b (st2_fn ()) st3_fn) -> MachineCmd b st1 st3_fn
    ma >> mb = ma >>= \() => mb


    data MachineIO : VendState -> Type where
         Do : {st1, st2_fn : _} -> MachineCmd a st1 st2_fn -> ((res : a) -> Inf (MachineIO (st2_fn res))) -> MachineIO st1

    namespace MachineDo
        export
        (>>=) : {st1, st2_fn : _} -> MachineCmd a st1 st2_fn -> ((res : a) -> Inf (MachineIO (st2_fn res))) -> MachineIO st1
        (>>=) = Do

        export
        %tcinline
        (>>) : {st1, st2_fn : _} -> MachineCmd () st1 st2_fn -> Inf (MachineIO (st2_fn ())) -> MachineIO st1
        ma >> mb = Do ma (\() => mb)


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
                            COIN => do cr <- InsertCoin
                                       case cr of
                                            Inserted => machineLoop
                                            Rejected => do Display "Coin rejected"                                       
                                                           machineLoop
                            VEND => vend
                            CHANGE => do GetCoins
                                         Display "Change returned"
                                         machineLoop
                            REFILL num => refill num
 