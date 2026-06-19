import Hangman
import General

promptStr :: String -> IO (String)
promptStr prompt = do
    putStr prompt
    putStr ": "
    val <- getLine
    return val

promptConfirm :: String -> IO (Bool)
promptConfirm prompt = do
    putStr prompt
    putStr " [y/N] "
    val <- getChar
    putStr "\n"
    if General.toLowerCase val == 'y'
        then return True
        else return False

promptStrUntilConfirmed :: String -> String -> IO (String)
promptStrUntilConfirmed prompt cprompt = do
    val <- promptStr prompt
    conf <- promptConfirm cprompt
    if conf
        then return val
        else promptStrUntilConfirmed prompt cprompt

linebreak = General.rep 50 "-"

gameLoop :: String -> Int -> Int -> String -> String -> IO (Bool)
gameLoop title maxLives curLives answer lastGuess = do
    putStrLn $ "\n" ++ linebreak
    putStr $ title ++ "\nLives: "
    putStrLn $ show curLives ++ "/" ++ show maxLives ++ "\n"
    putStrLn $ format answer (Hangman.wordCheckerWithHints answer lastGuess)
    
    curGuess <- promptStr "Enter a word or phrase"
    putStrLn linebreak
    let newLives = if length curGuess == 0 then curLives+1 else curLives

    let guessStates = Hangman.wordCheckerWithHints answer curGuess
    if length (filter (==Correct) guessStates) /= length answer
        then if newLives > 1
                then gameLoop title maxLives (newLives-1) answer curGuess
                else return False
        else return True

main :: IO ()
main = do
    --initial
    let gameName = "HANGMAN"
        gameLives = length gameName
    
    --word setup
    putStrLn linebreak
    putStrLn gameName
    answer <- promptStrUntilConfirmed "Enter a word or phrase to begin" "Confirm?"
    putStrLn linebreak
    --begin
    won <- gameLoop gameName gameLives gameLives answer ""
    if won
        then putStrLn "You won!"
        else putStrLn "Better luck next time!"
