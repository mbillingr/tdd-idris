module Main

import Data.String
import Data.Vect
import System.REPL

infixr 5 .+.

data Schema = SString
            | Sint
            | (.+.) Schema Schema

SchemaType : Schema -> Type
SchemaType SString = String
SchemaType Sint = Int
SchemaType (x .+. y) = (SchemaType x, SchemaType y)

record DataStore where
       constructor MkData
       schema : Schema
       size : Nat
       items : Vect size (SchemaType schema)


addToStore : (store : DataStore) -> SchemaType (schema store) -> DataStore
addToStore (MkData schema' size items) newitem = MkData schema' _ (addToData items)
    where
        addToData : Vect old (SchemaType schema') -> Vect (S old) (SchemaType schema')
        addToData [] = [newitem]
        addToData (item :: items) = item :: addToData items


data Command : Schema -> Type where
     Add : SchemaType schema' -> Command schema'
     Get : Integer -> Command schema'
     Quit : Command schema'


parsePrefix : (schema : Schema) -> String -> Maybe (SchemaType schema, String)
parsePrefix SString input = getQuoted (unpack input)
    where
        getQuoted : List Char -> Maybe (String, String)
        getQuoted ('"' :: xs) = case span (/= '"') xs of
                                     (quoted, '"' :: rest) => Just (pack quoted, ltrim (pack rest))
                                     _ => Nothing
        getQuoted _ = Nothing
parsePrefix Sint input = case span isDigit input of
                              ("", rest) => Nothing
                              (num, rest) => Just (cast num, ltrim rest)
parsePrefix (schemal .+. schemar) input = case parsePrefix schemal input of
                                               Nothing => Nothing
                                               Just (l_val, input') => 
                                                    case parsePrefix schemar input' of
                                                         Nothing => Nothing
                                                         Just (r_val, input'') => Just ((l_val, r_val), input'')

parseBySchema : (schema : Schema) -> String -> Maybe (SchemaType schema)
parseBySchema schema input = case parsePrefix schema input of
                                  Just (res, "") => Just res
                                  Just _ => Nothing
                                  Nothing => Nothing

parseCommand : (schema : Schema) -> String -> String -> Maybe (Command schema)
parseCommand schema "add" rest = case parseBySchema schema rest of
                                      Nothing => Nothing
                                      Just restok => Just (Add restok)
parseCommand _ "get" val = case all isDigit (unpack val) of
                                False => Nothing
                                True => Just (Get (cast val))
parseCommand _ "quit" "" = Just Quit
parseCommand _ _ _ = Nothing

parse : (schema : Schema) -> (input : String) -> Maybe (Command schema)
parse schema input = case span (/= ' ') input of
                          (cmd, args) => parseCommand schema cmd (ltrim args)


display : {schema' : Schema} -> SchemaType schema' -> String
display {schema' = SString} item = item
display {schema' = Sint} item = show item
display {schema' = (x .+. y)} (iteml, itemr) = display iteml ++ ", " ++ display itemr


getEntry : (store : DataStore) -> (pos : Integer) -> Maybe (String, DataStore)
getEntry store pos = let store_items = items store in 
                               case integerToFin pos (size store) of 
                                    Nothing => Just ("Out of Range\n", store)
                                    (Just id) => Just (display (index id store_items) ++ "\n", store)

processInput : DataStore -> String -> Maybe (String, DataStore)
processInput store inp
    = case parse (schema store) inp of
           Nothing => Just ("Invalid command\n", store)
           (Just (Add item)) => Just ("ID " ++ show (size store) ++ "\n", addToStore store item)
           (Just (Get pos)) => getEntry store pos
           (Just Quit) => Nothing

main : IO ()
main = replWith (MkData SString _ []) "Command: " processInput
