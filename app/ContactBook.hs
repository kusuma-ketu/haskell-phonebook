import Data.Maybe(from Maybe)

data Contact = Contact {
    name :: String,
    phone :: String
} deriving (Show, Eq)

type PhoneBook = [Contact]
addContact :: Contact -> PhoneBook -> PhoneBook
addContact contact book = contact : book

findContact :: String -> PhoneBook -> Maybe Contact
findContact _ [] = Nothing
findContact searchName (c:cs)
    | name c == searchName = Just c
    | otherwise = findContact searchName cs

showContact :: Maybe Contact -> String 
showContact Nothing = "Contact not found"
showContact (Just c) = "Name: " ++ c name ++ ", Phone: " ++ c phone