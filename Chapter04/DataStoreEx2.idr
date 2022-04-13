module Main

import Data.String
import Data.Vect
import System.REPL

data DataStore : Type where
     MkData : (size : Nat) ->
              (items : Vect size String) ->
              DataStore

size : DataStore -> Nat
size (MkData size' items') = size'

items : (store : DataStore) -> Vect (size store) String
items (MkData size' items') = items'

addToStore : DataStore -> String -> DataStore
addToStore (MkData size items) newitem = MkData _ (addToData items)
    where
        addToData : Vect old String -> Vect (S old) String
        addToData [] = [newitem]
        addToData (item :: items) = item :: addToData items


data Command = Add String
             | Get Integer
             | Size
             | Search String
             | Quit

parseCommand : String -> String -> Maybe Command
parseCommand "add" str = Just (Add str)
parseCommand "get" val = case all isDigit (unpack val) of
                              False => Nothing
                              True => Just (Get (cast val))
parseCommand "size" "" = Just Size
parseCommand "search" str = Just (Search str)
parseCommand "quit" "" = Just Quit
parseCommand _ _ = Nothing

parse : (input : String) -> Maybe Command
parse input = case span (/= ' ') input of
                   (cmd, args) => parseCommand cmd (ltrim args)


getEntry : (store : DataStore) -> (pos : Integer) -> Maybe (String, DataStore)
getEntry store pos = let store_items = items store in 
                               case integerToFin pos (size store) of 
                                    Nothing => Just ("Out of Range\n", store)
                                    (Just id) => Just (index id store_items ++ "\n", store)

searchEntries : String -> DataStore -> Maybe (String, DataStore)
searchEntries substr store = Just (search_results (findall (items store)), store)
    where
        search_results : List String -> String
        search_results items = foldl (++) "" (map (++ "\n") items)

        findall : Vect _ String -> List String
        findall [] = []
        findall (item :: items) = if isInfixOf substr item then
                                     item :: findall items else
                                     findall items

processInput : DataStore -> String -> Maybe (String, DataStore)
processInput store inp
    = case parse inp of
           Nothing => Just ("Invalid command\n", store)
           (Just (Add item)) => Just ("ID " ++ show (size store) ++ "\n", addToStore store item)
           (Just (Get pos)) => getEntry store pos
           (Just Size) => Just (show (size store) ++ " entries\n", store)
           (Just (Search substr)) => searchEntries substr store
           (Just Quit) => Nothing

main : IO ()
main = replWith (MkData _ []) "Command: " processInput