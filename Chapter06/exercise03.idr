module Main

import Data.List
import Data.String
import Data.Vect
import System.REPL

infixr 5 .+.

data Schema = SString
            | SInt
            | SChar
            | (.+.) Schema Schema

SchemaType : Schema -> Type
SchemaType SString = String
SchemaType SInt = Int
SchemaType SChar = Char
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
     GetAll : Command schema'
     Quit : Command schema'


parseSchema : List String -> Maybe Schema
parseSchema ("String" :: []) = Just SString
parseSchema ("String" :: xs) = do xs_sch <- parseSchema xs
                                  Just (SString .+. xs_sch)
parseSchema ("Int" :: []) = Just SInt
parseSchema ("Int" :: xs) = do xs_sch <- parseSchema xs
                               Just (SInt .+. xs_sch)
parseSchema ("Char" :: []) = Just SChar
parseSchema ("Char" :: xs) = do xs_sch <- parseSchema xs
                                Just (SChar .+. xs_sch)
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
parsePrefix SChar input = case unpack input of
                               [] => Nothing
                               (ch :: rest) => Just (ch, ltrim (pack rest))
parsePrefix (schemal .+. schemar) input = do (l_val, input') <- parsePrefix schemal input
                                             (r_val, input'') <- parsePrefix schemar input'
                                             Just ((l_val, r_val), input'')

parseBySchema : (schema : Schema) -> String -> Maybe (SchemaType schema)
parseBySchema schema input = case parsePrefix schema input of
                                  Just (res, "") => Just res
                                  Just _ => Nothing
                                  Nothing => Nothing

parseCommand : (schema : Schema) -> String -> String -> Maybe (Command schema)
parseCommand schema "add" rest = do restok <- parseBySchema schema rest
                                    Just (Add restok)
parseCommand _ "get" "" = Just GetAll
parseCommand _ "get" val = case all isDigit (unpack val) of
                                False => Nothing
                                True => Just (Get (cast val))
parseCommand _ "quit" "" = Just Quit
parseCommand schema "schema" rest = do schemaok <- parseSchema (words rest)
                                       Just (SetSchema schemaok)
parseCommand _ _ _ = Nothing

parse : (schema : Schema) -> (input : String) -> Maybe (Command schema)
parse schema input = case span (/= ' ') input of
                          (cmd, args) => parseCommand schema cmd (ltrim args)


display : {schema' : Schema} -> SchemaType schema' -> String
display {schema' = SString} item = item
display {schema' = SInt} item = show item
display {schema' = SChar} item = show item
display {schema' = (x .+. y)} (iteml, itemr) = display iteml ++ ", " ++ display itemr


getEntry : (store : DataStore) -> (pos : Integer) -> Maybe (String, DataStore)
getEntry store pos = let store_items = items store in 
                               case integerToFin pos (size store) of 
                                    Nothing => Just ("Out of Range\n", store)
                                    (Just id) => Just (display (index id store_items) ++ "\n", store)

listEntries : (store : DataStore) -> String
listEntries store = list_items 0 (size store) (items store)
    where
        list_items : Nat -> (n:Nat) -> Vect n (SchemaType (schema store)) -> String
        list_items _ Z [] = ""
        list_items id (S k) (x :: xs) = show id ++ ": " ++ display x ++ "\n" ++ list_items (S id) k xs

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
           (Just GetAll) => Just (listEntries store, store)
           (Just Quit) => Nothing

main : IO ()
main = replWith (MkData SString _ []) "Command: " processInput
