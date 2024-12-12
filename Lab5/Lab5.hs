-- This is the file for lab 5, a simlpe guessing game

data QA = Question String Sub Sub
    deriving Show
data Sub = SubQA QA | Person String 
    deriving Show

exampleQA = Question "Is it an actor?" 
    (Person "Person 1") (SubQA (Question "Is it a writer?" 
                        (Person "Person 2") (Person "Person 3")))

play :: QA -> IO QA
play = undefined

main :: IO ()
main = undefined