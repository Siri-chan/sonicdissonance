module Main where

import Control.Monad

import Control.Monad.State

import Data.List (sort)

import Data.Char (isLetter)

data HeaderState = HeaderState
  { player1 :: Player
  , player2 :: Player
  , deck :: Deck
  , turn :: Int
  }

data Player = Player
  { name :: String
  , hand :: Hand 
  , score :: Int
  }
type Hand = [Card]


drawCard :: State HeaderState Card
drawCard = do
  gs <- get
  let (c:cs) = deck gs
  put $ gs { deck = cs }
  return c

playCard :: Player -> Card -> Player
playCard p c = p { hand = filter (/= c) (hand p) }

scoreHand :: Hand -> Int
scoreHand h = sum (map scoreCard h)

scoreCard :: Card -> Int
scoreCard c = case rank c of
  Ace   -> 11
  King  -> 10
  Queen -> 10
  Jack  -> 10
  _     -> fromEnum (rank c) + 2

playTurn :: State HeaderState ()
playTurn = do
  gs <- get
  let p1 = player1 gs
      p2 = player2 gs
      t = turn gs
  card1 <- drawCard
  card2 <- drawCard
  let p1' = playCard p1 card1
      p2' = playCard p2 card2
      s1 = scoreHand (hand p1')
      s2 = scoreHand (hand p2')
      p1'' = p1' { score = score p1' + s1 }
      p2'' = p2' { score = score p2' + s2 }
      gs' = gs { player1 = p1'', player2 = p2'', turn = t + 1 }
  put gs'

playGame :: State HeaderState ()
playGame = do
  gs <- get
  if length (deck gs) < 2
    then return ()
    else do
      playTurn
      playGame

data Person = Person String Int

instance Show Person where
  show (Person name age) = name ++ " (" ++ show age ++ ")"


data Animal = Dog String Int
            | Cat String Int
            | Fish String

instance Show Animal where
    show (Dog name age) = "Dog named " ++ name ++ " (" ++ show age ++ ")"
    show (Cat name age) = "Cat named " ++ name ++ " (" ++ show age ++ ")"
    show (Fish name)    = "Fish named " ++ name

type Stack = [Int]

push :: Int -> State Stack ()
push x = modify (x:)

data Card = Card { suit :: Suit, rank :: Rank } deriving (Eq)

data Suit = Hearts | Diamonds | Clubs | Spades deriving (Eq, Ord)

data Rank = Ace | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King deriving (Eq, Ord, Enum)

instance Show Card where
  show (Card suit rank) = "E"

type Deck = [Card]

deal :: Deck -> Int -> ([Deck], Deck)
deal deck numHands = (hands, deck')
  where
    deck' = drop (numHands * 5) deck
    hands = take numHands $ chunk 5 deck

chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk n xs = take n xs : chunk n (drop n xs)
   
type WordCount = Int
type Word = String

countWords :: String -> [(Main.Word, WordCount)]
countWords input = 
    let wordsList = words $ filter isLetter $ map (\c -> if isLetter c then c else ' ') input
    in map (\w -> (w, length $ filter (==w) wordsList)) $ sort $ wordsList

animals :: [Animal]
animals = [Dog "Rufus" 3, Cat "Fluffy" 2, Fish "Nemo"]

people :: [Person]
people = [Person "Alice" 32, Person "Bob" 27, Person "Charlie" 35]

data Foo = Bar Int String
         | Baz [Char]

instance Show Foo where
    show (Bar x y) = "" ++ show x ++ "" ++ y
    show (Baz x)   = "This is an" ++ show x

foo :: Foo
foo = Baz " April Fools Joke"

quux :: Foo -> IO ()
quux x = putStrLn $ "" ++ show x



main :: IO ()
main = replicateM_ 10 $ quux foo