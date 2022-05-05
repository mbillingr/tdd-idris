import Data.Fuel
import Data.List
import Data.String
import Data.Vect
import Data.Vect.Elem
import Decidable.Equality

import Chapter09.RemoveElem


%default total

data GameState : Type where
     NotRunning : GameState
     Running : (guesses : Nat) -> (letters : Nat) -> GameState

data GuessResult = Correct | Incorrect

chars : String -> List Char
chars str = nub (map toUpper (unpack str))


data GameCmd : (ty : Type) -> GameState -> (ty -> GameState) -> Type where
     NewGame : (word : String) -> GameCmd () NotRunning (const (Running 6 (length (chars word))))
     Won  : GameCmd () (Running (S guesses) 0) (const NotRunning)
     Lost : GameCmd () (Running 0 (S letters)) (const NotRunning)

     Guess : (c : Char) -> GameCmd GuessResult (Running (S guesses) (S letters)) 
                                   (\res => case res of
                                                 Correct => Running (S guesses) letters
                                                 Incorrect => Running guesses (S letters))

     ShowState : GameCmd () state (const state)
     Message : String -> GameCmd () state (const state)
     ReadGuess : GameCmd Char state (const state)

     Pure : (res : ty) -> GameCmd ty (state_fn res) state_fn
     (>>=) : GameCmd a st1 st2_fn ->
             ((res : a) -> GameCmd b (st2_fn res) st3_fn) ->
             GameCmd b st1 st3_fn

(>>) : GameCmd () st1 st2_fn -> Inf (GameCmd b (st2_fn ()) st3_fn) -> GameCmd b st1 st3_fn
ma >> mb = ma >>= \() => mb


namespace Loop
    public export
    data GameLoop : (ty : Type) -> GameState -> (ty -> GameState) -> Type where
         (>>=) : GameCmd a st1 st2_fn ->
                 ((res : a) -> Inf (GameLoop b (st2_fn res) st3_fn)) ->
                 GameLoop b st1 st3_fn
         Exit : GameLoop () NotRunning (const NotRunning)

    export
    (>>) : GameCmd () st1 st2_fn -> Inf (GameLoop b (st2_fn ()) st3_fn) -> GameLoop b st1 st3_fn
    ma >> mb = ma >>= \() => mb

    export
    gameLoop : {letters : _} -> {guesses : _} -> GameLoop () (Running (S guesses) (S letters)) (const NotRunning)
    gameLoop {guesses} {letters} = do
        ShowState
        g <- ReadGuess
        ok <- Guess g
        case ok of
             Correct => case letters of
                             0 => do Won
                                     ShowState
                                     Exit
                             (S k) => do Message "Correct"
                                         gameLoop
             Incorrect => case guesses of
                               0 => do Lost
                                       ShowState
                                       Exit
                               (S k) => do Message "Incorrect"
                                           gameLoop

hangman : GameLoop () NotRunning (const NotRunning)
hangman = do NewGame "testing"
             gameLoop


data Game : GameState -> Type where
     GameStart  : Game NotRunning
     GameWon    : (word : String) -> Game NotRunning
     GameLost   : (word : String) -> Game NotRunning
     InProgress : {letters : _} -> (word : String) -> (guesses : Nat)
                  -> (missing : Vect letters Char)
                  -> Game (Running guesses letters)

Show (Game g) where
    show GameStart = "Starting"
    show (GameWon word) = "Game won: word was " ++ word
    show (GameLost word) = "Game lost: word was " ++ word
    show (InProgress word guesses missing) 
        = "\n" ++ pack (map hideMissing (unpack word))
               ++ "\n" ++ show guesses ++ " guesses left"
        where hideMissing : Char -> Char
              hideMissing c = if c `elem` missing then '-' else c

data GameResult : (ty : Type) -> (ty -> GameState) -> Type where
     Ok : (res : ty) -> Game (outstate_fn res) -> GameResult ty outstate_fn
     OutOfFuel : GameResult ty outstate_fn

ok : (res : ty) -> Game (outstate_fn res) -> IO (GameResult ty outstate_fn)
ok res st = pure (Ok res st)

runCmd : Fuel ->
         Game instate -> GameCmd ty instate outstate_fn ->
         IO (GameResult ty outstate_fn)
runCmd fuel state (NewGame word) = ok () (InProgress (toUpper word) _ (fromList (chars word)))
runCmd fuel (InProgress word _ missing) Won = ok () (GameWon word)
runCmd fuel (InProgress word _ missing) Lost = ok () (GameLost word)
runCmd fuel (InProgress word _ missing) (Guess c) 
    = case isElem c missing of
           (Yes prf) => ok Correct (InProgress word _ (removeElem c missing))
           (No contra) => ok Incorrect (InProgress word _ missing)
runCmd fuel state ShowState = do printLn state
                                 ok () state
runCmd fuel state (Message str) = do putStrLn str
                                     ok () state
runCmd (More fuel) st ReadGuess = do 
    putStr "Guess: "
    input <- getLine
    case unpack input of
         [x] => if isAlpha x
                   then ok (toUpper x) st
                   else do putStrLn "Invalid input"
                           runCmd fuel st ReadGuess
         _ => do putStrLn "Invalid input"
                 runCmd fuel st ReadGuess
runCmd Dry _ _ = pure OutOfFuel
runCmd fuel state (Pure res) = ok res state
runCmd fuel st (cmd >>= next) 
    = do Ok cmdRes newSt <- runCmd fuel st cmd
            | OutOfFuel => pure OutOfFuel
         runCmd fuel newSt (next cmdRes)

run : Fuel -> 
      Game instate ->
      GameLoop ty instate outstate_fn ->
      IO (GameResult ty outstate_fn)
run Dry _ _ = pure OutOfFuel
run (More fuel) st (cmd >>= next) = do Ok cmdRes newSt <- runCmd fuel st cmd
                                          | OutOfFuel => pure OutOfFuel
                                       run fuel newSt (next cmdRes)
run (More fuel) st Exit = ok () st


partial
main : IO ()
main = do _ <- run forever GameStart hangman
          pure ()