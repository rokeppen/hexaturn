module GameParser where

import           Control.Applicative
import           Control.Monad
import           Data.Char
import           Def
import           Methods
import qualified Data.Map                             as M
import           Data.List (nub)
import           Data.Containers.ListUtils (nubOrdOn)

-- The type of parsers
newtype Parser a = Parser { f :: String -> [(a, String)] }

-- Return parsed value, assuming at least one successful parse
parse :: Parser a -> String -> Either String a
parse m s  =  one [ x | (x, t) <- f m s, t == "" ]
  where one []  = Left "no parse"
        one [x] = Right x
        one _   = Left "ambiguous parse"

instance Functor Parser where
  fmap = liftM

instance Applicative Parser where
  pure  = return
  (<*>) = ap

instance Alternative Parser where
  (<|>) = mplus
  empty = mzero

instance Monad Parser where
  return x = Parser $ \s -> [ (x, s) ]
  m >>= k  = Parser $ \s -> [ (y, u) | (x, t) <- f m s, (y, u) <- f (k x) t ]

instance MonadPlus Parser where
  mzero     = Parser $ const []
  mplus m n = Parser $ \s -> f m s ++ f n s

-- Parse one character
char :: Parser Char
char = Parser p
  where p []    = []
        p (c:s) = [(c,s)]

-- Parse a character satisfying a predicate (e.g., isDigit)
spot :: (Char -> Bool) -> Parser Char
spot p = do c <- char
            guard (p c)
            return c

-- Match a given character
token :: Char -> Parser Char
token c = spot (== c)

-- match a given string
match :: String -> Parser String
match = mapM token

-- match zero or more occurrences
star :: Parser a -> Parser [a]
star p = plus p `mplus` return []

-- match one or more occurrences
plus :: Parser a -> Parser [a]
plus p = do x <- p
            xs <- star p
            return (x:xs)

-- match n or more occurences
nplus :: Int -> Parser a -> Parser [a]
nplus 0 p = star p
nplus n p = do x <- p
               xs <- nplus (n-1) p
               return (x:xs)

-- match a natural number
parseNat :: Parser Int
parseNat = read <$> plus (spot isDigit)

-- match a negative number
parseNeg :: Parser Int
parseNeg = do n <- token '-' >> parseNat
              return (-n)

-- match an integer
parseInt :: Parser Int
parseInt = parseNat `mplus` parseNeg

-- match an occurence from one of the given parsers
parseOneOf :: [Parser a] -> Parser a
parseOneOf [x]    = x
parseOneOf (x:xs) = x `mplus` parseOneOf xs

-- match an occurence of a string from a list
parseListElem :: [String] -> Parser String
parseListElem x = parseOneOf $ map match x 

-- match an end-of-line-character
parseEnd :: Parser String
parseEnd = parseListElem ["\r","\n","\r\n"]

-- match content between brackets
parseBrackets :: Parser a -> Parser a
parseBrackets p = do x <- token '(' >> p
                     token ')' >> return x

-- match an item header
parseItem :: Parser a -> Parser a
parseItem p = do x <- token '*' >> plus (token ' ') >> p
                 token ':' >> (plus (token ' ') `mplus` match "\t") >> return x

-- match one or more occurences separated by a delimiter
parseWithDelim :: Parser a -> Parser b -> Parser [a]
parseWithDelim p1 p2 = do x <- p1
                          xs <- star (p2 >> p1)
                          return (x:xs)

-----------------
-- Game Parser --
-----------------

-- match a line where a power is descibed
parsePower :: Parser (Power,Int)
parsePower = do x <- parseItem $ parseListElem ["freeze","double tap"]
                i <- parseNat
                case i of
                     0 -> return []
                     1 -> match " charge"
                     _ -> match " charges"
                parseEnd
                if x == "freeze" then return (Freeze,i) else return (DoubleTap,i)

-- matches multiple lines with a power
parsePowerLines :: Parser Powers
parsePowerLines = M.fromList <$> (parseEnd >> plus parsePower)

-- match the empty powers
parseNone :: Parser Powers
parseNone = match " None" >> parseEnd >> return M.empty

-- match the power description block
parsePowers :: Parser Powers
parsePowers = match "powers:" >> (parseNone `mplus` parsePowerLines)

-- methods to transform zones into coordinates
lineFromCoord :: [Int] -> [Coord]
lineFromCoord (x1:y1:x2:y2:_) | x1 == x2  = [(x1,y) | y <- [min y1 y2..max y1 y2]] 
                              | y1 == y2  = [(x,y1) | x <- [min x1 x2..max x1 x2]]
                              | otherwise = [(x1+t,y1+t) | t <- [0..abs (x2-x1)]] 

adjacentCoord :: Int -> [Coord] -> [Coord]
adjacentCoord 0 c = c
adjacentCoord r c = nub $ c ++ adj ++ adjacentCoord (r-1) adj
                    where adj          = nub $ concatMap border c
                          border (x,y) = [(x,y-1),(x,y+1),(x-1,y),(x-1,y+1),(x+1,y),(x+1,y-1)]

-- match the coordinates of a zone
parseCoord :: Parser [Int]
parseCoord = parseBrackets $ parseWithDelim parseInt (match ", ")

-- match a cell zone
parseCell, parseLine, parseHexagon, parseTilted, parseForward, parseBackward, parseTube :: Parser [Coord]
parseCell     = do c <- match "cell" >> parseCoord
                   guard (length c == 2)
                   return [(c!!0,c!!1)]
parseLine     = do c <- match "line" >> parseCoord
                   guard (length c == 4)
                   return $ lineFromCoord c
parseHexagon  = do c <- match "hexagon" >> parseCoord
                   guard (length c == 3)
                   return $ adjacentCoord (abs (c!!2)) [(c!!0,c!!1)]
parseTilted   = do c <- match "tilted parallelogram" >> parseCoord
                   guard (length c == 4)
                   return $ concat [lineFromCoord [c!!0+t,c!!1,c!!0-c!!2+t,c!!1+c!!2] | t <- [0..c!!3]]
parseForward  = do c <- match "backward parallelogram" >> parseCoord
                   guard (length c == 4)
                   return $ concat [lineFromCoord [c!!0+t,c!!1-t,c!!0+t,c!!1+c!!2-t] | t <- [0..c!!3]]
parseBackward = do c <- match "forward parallelogram" >> parseCoord
                   guard (length c == 4)
                   return $ concat [lineFromCoord [c!!0+t,c!!1,c!!0+t,c!!1-c!!2] | t <- [0..c!!3]]
parseTube     = do c <- match "tube" >> parseCoord
                   guard (length c == 5)
                   return $ adjacentCoord (abs (c!!4)) $ lineFromCoord $ take 4 c

-- match a celltype an convert the given zone to the celltype
parseNormal, parseBlocked, parseNonblockable, parseOpen, parseTarget, parseRobot :: [Coord] -> Parser [Cell]
parseNormal c       = match "normal" >> (return $ map Unblocked c)
parseBlocked c      = match "blocked" >> (return $ map Blocked c)
parseNonblockable c = match "nonblockable" >> (return $ map Nonblockable c)
parseOpen c         = match "open" >> (return $ map Open c)
parseTarget c       = match "target" >> (return $ map Target c)
parseRobot c        = do s <- match "robot" >> parseBrackets parseInt
                         guard (s>0)
                         return $ map (\x -> Robot x s False) c 

-- match a single gridline
parseGridLine :: Parser [Cell]
parseGridLine = do c <- parseItem $ parseOneOf [parseTube,parseBackward,parseForward,parseTilted,parseHexagon,parseLine,parseCell]
                   cells <- parseOneOf $ map ($c) [parseNormal, parseBlocked,parseNonblockable,parseOpen,parseTarget,parseRobot]
                   parseEnd >> return cells

-- match a grid desciption
parseGrid :: Parser HgC
parseGrid = do cells <- nubOrdOn getPos . concat . reverse <$> (match "cells:" >> parseEnd >> plus parseGridLine)
               return (Grid cells False)

-- match a Game
parseGame :: Parser (Game HgC)
parseGame = do powers <- parsePowers
               grid <- parseEnd >> parseGrid
               let firstcoord = getPos $ head $ cells grid
               return (Game grid (Cursor firstcoord) powers)

-- match the line between the descriptions of games
parseDashed :: Parser ()
parseDashed = nplus 3 (token '-') >> parseEnd >> return ()

-- match a levelfile
parseLevels :: Parser [Game HgC]
parseLevels = parseWithDelim parseGame parseDashed 

--------------------
-- Command Parser --
--------------------

data Command = Click | Frozen | Double

-- match a click command, a frozen command or a double tap command and check if it is valid in the given game
parseClick, parseFrozen, parseDouble :: Game HgC -> Parser (Command,Coord)
parseClick game  = do c <- match "click " >> parseWithDelim parseInt (token ' ')
                      guard (length c == 2 && isTappable (grid game) (c!!0,c!!1))
                      return (Click,(c!!0,c!!1))
parseFrozen game = do c <- match "freeze " >> parseWithDelim parseInt (token ' ')
                      guard (length c == 2 && charge Freeze (powers game) > 0)
                      let newgame = usePower Freeze game
                      guard (isTappable (grid newgame) (c!!0,c!!1))
                      return (Frozen,(c!!0,c!!1))
parseDouble game = do c <- match "double tap " >> parseWithDelim parseInt (token ' ')
                      guard (length c == 2 && charge DoubleTap (powers game) > 0 && isTappable (grid game) (c!!0,c!!1))
                      return (Double,(c!!0,c!!1))

-- match a command for a given game
parseCommand :: Game HgC -> Parser (Command,Coord)
parseCommand game = parseOneOf $ map ($game) [parseClick,parseFrozen,parseDouble]

