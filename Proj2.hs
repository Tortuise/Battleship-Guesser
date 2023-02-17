-- Name: Kenny Ung Id: 1127378

------------------Summary--------------------------
-- COMP30020 Project 2 Battleship Guessing Game
-- This program is a Haskell program where two players play a guessing game
-- of battleship.. Given a gamestate of a 4x8 grid, the program will act
-- as the searcher and hider and will try to guess the locations of 
-- the hidden ships aswell as give feedback to the accuracy of the guess
-- until all ships are found.
---------------------------------------------------

-------------------------Purpose------------------------------
-- The purpose of this file is to simulate the game in which there are three 
-- locations where the battleships are hidden. 
-- The objective of the hider is to hide their ships in three different 
-- locations and the objective of the searcher is to find those ships in 
-- as few guesses as possible.
-- The searcher will call an initialGuess function in which feedback will 
-- be given to include three numbers: 
-- 1. Number of ships exactly located.
-- 2. Number of guesses that were exactly one space away from a ship.
-- 3. Number of guesses that were exactly two spaces away from a ship.
-- The searcher will then call a next guess given the feedback from 
-- the previous guess and updated gameState as input and will be called 
-- repeatedly until guessed correctly.
-- Each guess is counted as its closest distance to any ship.
-- Each guess consists of exactly three different locations.
-- GameState type will be defined to represent list of candidate targets 
-- remaining and locations as a list of lists where an empty square, Guess and 
-- Ship are represented as ' ', 'G' and 'S' respectively.
-- Location types are defined to represent grid locations in the game as an 
-- Eq type class they are notated by a letter from A-H denoting coloumn and a 
-- number 1-4 denoting the row.
-- Each guess made will trim the gamestate by filtering for candidate targets
-- that only give the same feedback, then each candidate target is simulated
-- as a guess against the rest of candidate targets as targets
-- generating a list of list of feedbacks which the expected result formula
-- will be used to get expected number of remaining candidates left from each
-- simulated guess choosing the guess with lowest remaining candidates left.
---------------------------------------------------

module Proj2 (Location, toLocation, fromLocation, feedback,
              GameState, initialGuess, nextGuess) where
              
-----------------Libraries--------------
import Data.List
import Data.Maybe
-----------------------------------------

---------------Type Definitions and global variables----------
type Location = (Int, Int)
type GameState = [[Location]] 
falseLoc = (-1,-1)::Location
allLocs = [
           (1,1),(1,2),(1,3),(1,4),
           (2,1),(2,2),(2,3),(2,4),
           (3,1),(3,2),(3,3),(3,4),
           (4,1),(4,2),(4,3),(4,4),
           (5,1),(5,2),(5,3),(5,4),
           (6,1),(6,2),(6,3),(6,4),
           (7,1),(7,2),(7,3),(7,4),
           (8,1),(8,2),(8,3),(8,4)]::[Location] -- used to generate 4960 combs
-----------------------------------------

-----------------------------------------
--           Functions
-----------------------------------------

-------------------------------
-- Location and its helper functions
-------------------------------

-- gives Just the Location named by the string, or Nothing if the string is 
-- not a valid location name.
toLocation :: String -> Maybe Location
toLocation (letter:num)
    | checkLoc (letter:num) = Just ((fromJust letterNum)+1, (read num::Int))
    | otherwise = Nothing
    where letterNum = convertLetterNum letter
    
-- gives back the two-character string version of the specified location; 
-- for any location loc, toLocation (fromLocation loc) should return Just loc.
fromLocation :: Location -> String
fromLocation (letter, num) = numLetter:(show num)
    where numLetter = convertNumLetter (letter-1)

-- converts letter of location into int
convertLetterNum :: Char -> Maybe Int
convertLetterNum x = (elemIndex x "ABCDEFGH")

-- Helper func to check if letter in alphabet range
-- and number in number range
checkLoc :: String -> Bool
checkLoc (letter:num) = (elem letter "ABCDEFGH") && (elem (read num::Int) [1..4])
   
-- converts number of location into char
convertNumLetter :: Int -> Char
convertNumLetter x = "ABCDEFGH"!! x
 
-------------------------------
-- feedback and its helper functions
-------------------------------

-- takes a target and a guess, respectively, and returns the appropriate 
-- feedback, as specified above.
feedback :: [Location] -> [Location] -> (Int,Int,Int)
feedback target guesses = (length exact,length one ,length two)
    where 
    exact = exactGuesses target guesses
    one = oneGuesses target guesses exact
    two = twoGuesses target guesses exact one

-- takes target and guesses returns list locations of guesses that are exact
exactGuesses :: [Location] -> [Location] -> [Location]
exactGuesses locs [] = []
exactGuesses locs (y:ys) 
    | elem y locs = y:exactGuesses locs ys
    | otherwise = exactGuesses locs ys

-- takes target, guesses and guesses that are exact, checks if 1 square away
-- from target and check if it is an exact location, if so add to list of loc
oneGuesses :: [Location] -> [Location] -> [Location] -> [Location]
oneGuesses locs [] exacts = []
oneGuesses locs (x:xs) exacts 
    | elem x exacts = oneGuesses locs xs exacts -- skip if on exact 
    | elem loc locs && loc /= falseLoc = x:oneGuesses locs xs exacts 
    | otherwise = oneGuesses locs xs exacts
    where
        loc = findOne x locs
        
-- given a target and guesses check if a guess is within one square
-- of a target and return that guess
findOne :: Location -> [Location] -> Location
findOne guess [] = falseLoc -- false location signify no location found
findOne guess (loc:locs)
    |  d == 1 = loc
    | otherwise = findOne guess locs
    where d = checkDistance guess loc
    
-- takes target, guesses, guesses that are exact and guesses within one square 
-- and returns guesses that are two squares within any target
twoGuesses :: [Location] -> [Location] -> [Location] -> [Location]-> [Location]
twoGuesses locs [] exacts ones = []
twoGuesses locs (x:xs) exacts ones
    | elem x exacts = twoGuesses locs xs exacts ones -- skip if on exact 
    | elem x ones  = twoGuesses locs xs exacts ones -- skip if already 1 tile away
    | elem loc locs = x:twoGuesses locs xs exacts ones
    | otherwise = twoGuesses locs xs exacts ones
    where
        loc = findTwo x locs

-- given a target and guesses check if a guess is within two squares
-- of a target and return that guess   
findTwo :: Location -> [Location] -> Location
findTwo guess [] = falseLoc -- false location signify no location found
findTwo guess (loc:locs)
    |  d == 2 = loc
    | otherwise = findTwo guess locs
    where d = checkDistance guess loc
    
-- given a guess and a location,
-- returns Int of 1 or 2 if distance is either one or two squares from eachother
-- if further than two squares or exact return a -1
checkDistance :: Location -> Location -> Int
checkDistance (x1,y1) (x2,y2) 
    | d == 1 = 1
    | d == 2 = 2
    | otherwise = -1
    where 
        d = floor (sqrt (z))
        z = xd^2+yd^2
        xd = fromIntegral (x2 - x1)
        yd = fromIntegral (y2 - y1)

-----------------------------------
-- initialGuess and its helper functions
-----------------------------------

-- takes no input arguments, and returns a pair of an initial 
-- guess and a game state.
-- good first guess will be one where gives (0,0,0)
-- first attempt: choose bottom corners and top middle
initialGuess :: ([Location],GameState)
initialGuess = (guess, game)
    where 
        game = (combinations 3 allLocs)::GameState
        guess = [(1,1),(1,2),(1,3)]::[Location] -- for now hardcode corners
        -- computed best guess [(1,1),(1,2),(1,3)]
        

-- general function to generate combinations n List 
combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations _ [] = []
combinations n (x:xs) = (map (x:) (combinations (n-1) xs)) ++ (combinations n xs)

-----------------------------------
-- nextGuess and its helper functions
-----------------------------------

-- nextguess is called after initialGuess until finds all locations of ship
-- inputs pair (prev guess, gameState) and feedback of target called in Main.hs
-- outputs next guess based on guess with lowest expected remaining candidates
-- determined by trimming gameState by filtering only those that give same
-- feedback as feedback from real target.
nextGuess :: ([Location],GameState) -> (Int,Int,Int) -> ([Location],GameState)
nextGuess (prevGuess,game) fb = (bestGuess,deleteBestGuess)
    where
        trimmed = sameFeedback (prevGuess,game) fb
        feedbacks = checkGuess trimmed trimmed
        groupedFeedbacks = map sortGroupFeedback feedbacks
        expectedList = traverseFeedbacks groupedFeedbacks 
        maxIndex = fromJust (elemIndex (minimum expectedList) expectedList)
        bestGuess = trimmed!!maxIndex
        deleteBestGuess = delete bestGuess trimmed

-- takes in pair prev guess and ggameState and feedback
-- returns list of locations that are consistent with feedback input and prev
-- guess to trim down list of candidates
sameFeedback :: ([Location],GameState) -> (Int,Int,Int) -> GameState
sameFeedback (_,[]) _ = []
sameFeedback (prevGuess,(t:ts)) fb 
    | feedback t prevGuess == fb = t:sameFeedback (prevGuess,ts) fb
    | otherwise = sameFeedback (prevGuess,ts) fb
    
-- input candidate list and whole candidate list, traverse thru all
-- candidate list go thru all candidate List and make it a guess
-- go thru all candidates and store feedback for all cList[i]
checkGuess :: GameState -> GameState -> [[(Int,Int,Int)]]
checkGuess [] _ = []
checkGuess (g:gs) game = listFeedback:checkGuess gs game
    where
        listFeedback = map (`feedback` g) game

        
-- traverses list of grouped feedbacks and inputs list and total into 
-- expectedAverage function
traverseFeedbacks :: [[Int]] -> [Double]
traverseFeedbacks [] = []
traverseFeedbacks (f:fs) = result:traverseFeedbacks fs
    where
        total = length f
        result = expectedAverage f total

-- formula function to compute expected number of remaining candidates
-- takes in input list of distinct feedbacks and outputs the result based on 
-- formula ∑f∈F (count(f)^2)/2
expectedAverage :: [Int] -> Int -> Double
expectedAverage [] total = 0
expectedAverage (f:fs) total = (countf2 / t) + (expectedAverage fs total)
    where
        countf2 = fromIntegral (f^2)
        t = fromIntegral total
        
-- sorts list of tuple of feedback and group them into number of 
-- distinct feedbacks        
sortGroupFeedback :: [(Int, Int, Int)] -> [Int]
sortGroupFeedback feedbacks = map length (group (sort feedbacks))

-------------------------------------
-- get best initial guess functions
-------------------------------------

-- create all combinations of guesses and compute how many number of
-- (0,0,0) feedback is generated for each guess and choose guess with
-- highest number of (0,0,0) feedback resulting in [(1,1),(1,2),(1,3)]
getBestGuess :: [Location]
getBestGuess = bestGuess
    where 
        game = (combinations 3 allLocs)::[[Location]]
        lengthAllZeros = checkAllGuess game game
        maxIndex = fromJust (elemIndex (maximum lengthAllZeros) lengthAllZeros)
        bestGuess = game!!maxIndex

-- input candidate list and whole candidate list, traverse thru all
-- candidatelist go thru all cList and make it a guess
-- after trimming go thru all candidates and store feedback for all cList[i]
checkAllGuess :: GameState -> GameState -> [Int]
checkAllGuess [] _ = []
checkAllGuess (g:gs) game = x:checkAllGuess gs game
    where 
        listFeedback = map (`feedback` g) game 
        filtered = filter allzero listFeedback
        x = length filtered  
        
-- filter for only certain feedbacks within or further than two squares     
allzero :: (Int,Int,Int) -> Bool
allzero x = x == (0,0,0)||x==(0,0,3)||x==(0,0,2)||x==(0,0,1)

---------------------
-- testing functions
---------------------

-- test code to get average number of guesses for all combinations
testAverage :: IO ()
testAverage = listGuesses
    where
        (testG,testGame) = initialGuess
        listGuesses = testAllGuess testG testGame testGame 1 []

-- takes in initial guess, initial gamestate twice, guess # and list of averages
-- [] initially, then loops through all possible 4960 combinations
-- as a target and outputs number guesses to find target aswell
-- as average throughout testing all targets
testAllGuess :: [Location] -> GameState -> GameState -> Int -> [Int] -> IO ()
testAllGuess _ [] _ guessNum guesses = putStrLn $ "end " ++  
    show ((sum (guesses)) `div` (guessNum))
testAllGuess testG (g:gs) game guessNum guesses = do
    { putStrLn $ "guess #" ++ show guessNum ++ ":  " ++ showLocations g ++
    " in " ++ show x ++ " guesses, average: " ++ show average
    ; testAllGuess testG gs game (guessNum+1) (x:guesses)
    }
    where 
        x = loop g testG game 1
        average = (sum (x:guesses)) `div` (guessNum)
 
-- test code loop thru given target until get ans returns num of guesses
-- takes in target , guess, gamestate and number guesses
-- borrows code taken from Main.hs
loop :: [Location] -> [Location] -> GameState -> Int -> Int
loop target guess other guesses = do
  let answer = feedback target guess
  if answer == (3,0,0)
    then do
      guesses
    else do
      let (guess',other') = nextGuess (guess,other) answer
      loop target guess' other' (guesses+1)

-- borrowed from Main.hs outputs location as string
showLocations :: [Location] -> String
showLocations = unwords . (fromLocation <$>)