import Test.HUnit
import Data.Maybe (isNothing, fromJust)
import ContactBook  -- assumes your main code is in ContactBook.hs

-- Sample contacts
alice = Contact "Alice" "123-456"
bob   = Contact "Bob" "987-654"

-- 1. Test addContact
testAddContact :: Test
testAddContact = TestCase $
  let book = addContact alice []
  in assertBool "Book should contain Alice" (alice `elem` book)

-- 2. Test findContact for existing name
testFindExisting :: Test
testFindExisting = TestCase $
  let book = addContact bob [alice]
  in assertEqual "Should find Bob" (Just bob) (findContact "Bob" book)

-- 3. Test findContact for non-existent name
testFindMissing :: Test
testFindMissing = TestCase $
  let book = [alice]
  in assertBool "Should return Nothing" (isNothing (findContact "Charlie" book))

-- Group all tests
tests :: Test
tests = TestList
  [ TestLabel "Add Contact" testAddContact
  , TestLabel "Find Existing" testFindExisting
  , TestLabel "Find Missing" testFindMissing
  ]

main :: IO ()
main = do
  counts <- runTestTT tests
  if failures counts > 0 || errors counts > 0
    then putStrLn "❌ Some tests failed"
    else putStrLn "✅ All tests passed!"
