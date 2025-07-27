import Data.Maybe(from Maybe)
import Data.Char (toLower)
import System.IO (writeFile, readFile)


findContact :: String -> PhoneBook -> Maybe Contact
findContact _ [] = Nothing
findContact searchName (c:cs)
    | normalize name c == normalize searchName = Just c
    | otherwise = findContact normalize searchName cs

showContact :: Maybe Contact -> String 
showContact Nothing = "Contact not found"
showContact (Just c) = "Name: " ++ c name ++ ", Phone: " ++ c phone

updateContact :: String -> String -> PhoneBook -> PhoneBook
updateContact _ _ [] = []
updateContact searchName newPhone (c:cs)
    | name c == searchName = Contact (name c) newPhone : cs
    | otherwise = c : updateContact searchName newPhone cs

deleteContact :: String -> PhoneBook -> PhoneBook
deleteContact _ [] = []
deleteContact searchName (c:cs)
    | name c == searchName = cs
    | otherwise = c : deleteContact searchName cs

saveContacts :: FilePath -> PhoneBook -> IO()
saveContacts path book = wrtieFile path (show book)

loadContacts :: FilePath -> IO PhoneBook
loadContacts path = do
    content <- readFile path
    return (read content :: PhoneBook)