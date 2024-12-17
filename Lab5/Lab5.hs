-- This is the file for lab 5, a simlpe guessing game
import Text.Read
import System.IO.Error (tryIOError)
import Data.Char

data QA = Question String QA QA | Person String
    deriving (Read,Show)

-- The base case the game will use if there is no file or the file is corrupted
baseCase = Question "Is this person from Europe?"
    (Question "Is this person a scientist?" 
        (Person "Marie Curie") (Person "Queen Elisabeth"))
    (Question "Is this person an actor?" 
        (Person "Marilyn Monroe") (Person "Hillary Clinton"))

main :: IO ()
main = do
    putStrLn $ "Welcome to my guessing game, " ++
               "I will guess what person you are thinking about!"
    qa <- getQA
    startGame qa
    print "End of game, thank you for playing!"

-- Tries to read a file, if successful check that the contents of the file is 
-- valid, if so, use that for the game, 
-- otherwise returns and use the base case.
getQA :: IO QA
getQA = do
    fileContent <- tryIOError (readFile "questions.qa")
    case fileContent of 
        Left  _       -> return baseCase
        Right content -> case (readMaybe content) of
            Just a -> return a
            _      -> return baseCase

-- Creates a loop where the user can play the game as many times as they want,
-- if they are done playing, call function that saves the new data
startGame :: QA -> IO ()
startGame qa = do
    newQa <- play qa
    putStrLn "Play again?"
    gameState <- inputHandler
    case gameState of
        "yes" -> startGame newQa
        "no"  -> exitGame newQa

-- Function that save the new QA (qa) as a String in the file "questions.qa"
exitGame:: QA -> IO ()
exitGame qa = do
    writeFile "questions.qa" (show qa)


-- The function that handles the functionality of the game
play :: QA -> IO QA
-- The case of the play where the input is the edge case (a person)
-- where the computer will guess a person and the functionality 
-- for that case is handled
play (Person p) = do
    putStrLn $ "My guess: Is it " ++ show p ++ "?"
    input <- inputHandler
    case input of 
        "yes" -> do
            putStrLn "Hurray! I won!"
            return (Person p)
        "no"  -> do
            putStrLn $ "OK - you won this time.\n" ++ 
                       "Just curious: Who was your famous person?" 
            newPerson <- getLine
            putStrLn $ "Give me a question for which the answer for " 
                       ++ newPerson ++ " is \"yes\" and the answer for " 
                       ++ p ++ " is \"no\"."
            newQuestion <- getLine
            return (Question newQuestion (Person newPerson) (Person p))
    where inPerson = do
            answerPerson <- getLine
            return answerPerson

-- The case of the play where the input is a tree in the form of a QA
play (Question q sub1 sub2) = do
    putStrLn q
    input <- inputHandler
    case input of 
        "yes" -> do
            newqa <- play sub1
            return (Question q newqa sub2)
        "no"  -> do
            newqa <- play sub2
            return (Question q sub1 newqa)

-- A function where the input from the user is handled to be only
-- "yes" or "no"
inputHandler :: IO String
inputHandler = do
    answer <- getLine
    let lowerAnswer = map toLower answer
    case lowerAnswer of
        "yes" -> do
            return "yes"
        "no"  -> do
            return "no"
        _     -> do
            putStrLn "Please answer yes or no!"
            inputHandler