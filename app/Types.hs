data Contact = Contact {
    name :: String,
    phone :: String
} deriving (Show, Eq)

normalize :: String -> String
normalize = map toLower

type PhoneBook = [Contact]
addContact :: Contact -> PhoneBook -> PhoneBook
addContact contact book = contact : book