module Main

import Data.List
import Data.String
import Data.Vect
import System.REPL

infixr 5 .+.

data Schema = SString
            | SInt
            | (.+.) Schema Schema

SchemaType : Schema -> Type
SchemaType SString = String
SchemaType SInt = Int
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
     SetSchema : (newschema : Schema) -> Command schema'
     Add : SchemaType schema' -> Command schema'
     Get : Integer -> Command schema'
     Quit : Command schema'


parseSchema : List String -> Maybe Schema
parseSchema ("String" :: xs) 
    = case xs of 
           [] => Just SString
           _ => case parseSchema xs of
                     Nothing => Nothing
                     Just xs_sch => Just (SString .+. xs_sch)
parseSchema ("Int" :: xs)
    = case xs of 
           [] => Just SInt
           _ => case parseSchema xs of
                     Nothing => Nothing
                     Just xs_sch => Just (SInt .+. xs_sch)
parseSchema _ = Nothing


parsePrefix : (schema : Schema) -> String -> Maybe (SchemaType schema, String)
parsePrefix SString input = getQuoted (unpack input)
    where
        getQuoted : List Char -> Maybe (String, String)
        getQuoted ('"' :: xs) = case span (/= '"') xs of
                                     (quoted, '"' :: rest) => Just (pack quoted, ltrim (pack rest))
                                     _ => Nothing
        getQuoted _ = Nothing
parsePrefix SInt input = case span isDigit input of
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
parseCommand schema "schema" rest = case parseSchema (words rest) of
                                         Nothing => Nothing
                                         Just schemaok => Just (SetSchema schemaok)
parseCommand _ _ _ = Nothing

parse : (schema : Schema) -> (input : String) -> Maybe (Command schema)
parse schema input = case span (/= ' ') input of
                          (cmd, args) => parseCommand schema cmd (ltrim args)


display : {schema' : Schema} -> SchemaType schema' -> String
display {schema' = SString} item = item
display {schema' = SInt} item = show item
display {schema' = (x .+. y)} (iteml, itemr) = display iteml ++ ", " ++ display itemr


getEntry : (store : DataStore) -> (pos : Integer) -> Maybe (String, DataStore)
getEntry store pos = let store_items = items store in 
                               case integerToFin pos (size store) of 
                                    Nothing => Just ("Out of Range\n", store)
                                    (Just id) => Just (display (index id store_items) ++ "\n", store)

setSchema : (store : DataStore) -> Schema -> Maybe DataStore
setSchema store schema = case size store of
                              Z => Just (MkData schema _ [])
                              _ => Nothing

processInput : DataStore -> String -> Maybe (String, DataStore)
processInput store inp
    = case parse (schema store) inp of
           Nothing => Just ("Invalid command\n", store)
           (Just (SetSchema schema')) => case setSchema store schema' of 
                                              Nothing => Just ("Can't update schema\n", store)
                                              (Just store') => Just ("Ok\n", store')
           (Just (Add item)) => Just ("ID " ++ show (size store) ++ "\n", addToStore store item)
           (Just (Get pos)) => getEntry store pos
           (Just Quit) => Nothing

main : IO ()
main = replWith (MkData SString _ []) "Command: " processInput
