module ErrorParser where

import           Data.Containers.ListUtils (nubOrdOn)
import           Data.Either (isRight)
import           Control.Applicative
import           Control.Monad
import           Data.Char
import           Def
import           Methods
import           GameParser (Parser(..),lineFromCoord,adjacentCoord)
import qualified Data.Map              as M
import           Control.Monad.Except
import           Data.List (sortOn)

type ErrorParser = ExceptT Error Parser

data Error = CharError | SpotError | TokenError Char | MatchError String | IntError | MissingEndError | BracketsError | ItemError | InvalidPowerError Error | InvalidChargeError | WrongConjugateError | PowerHeadError | NonePowerError | InvalidZoneNameError | InvalidZoneError Error | MismatchingZoneError String | InvalidTypeError | InvalidRobotSpeedError Error | ZeroRobotSpeedError | CellHeadError | DashError

instance Show Error where
   show CharError                          = "no character found"
   show SpotError                          = "character does not meet predicate"
   show (TokenError t)                     = "character does not equal " ++ show t
   show IntError                           = "string does not represent an integer"
   show (MatchError s)                     = "string does not equal " ++ s
   show MissingEndError                    = "expected an EOL-character"
   show BracketsError                      = "missing bracket"
   show ItemError                          = "wrong itemisation style"
   show InvalidChargeError                 = "charge must be a natural number bigger than 0"
   show (InvalidPowerError ItemError)      = "could not parse power: " ++ show ItemError
   show (InvalidPowerError (MatchError _)) = "could not parse power: unknown power"
   show WrongConjugateError                = "wrong conjugate of 'charge' after the charge number"
   show PowerHeadError                     = "power section does not begin with 'powers:'"
   show NonePowerError                     = "power section without powers must end with 'None'"
   show InvalidZoneNameError               = "unknown zone name"
   show (InvalidZoneError e)               = "invalid zone: " ++ show e
   show (MismatchingZoneError s)           = "mismatching number of integers with zone " ++ s
   show InvalidTypeError                   = "unknow type name" 
   show (InvalidRobotSpeedError e)         = "invalid robot speed: " ++ show e
   show ZeroRobotSpeedError                = "the speed of a robot can not be 0"
   show CellHeadError                      = "cell section does not begin with 'cells:'"
   show DashError                          = "levels must have a dashed line between then from at least 3 characters"

-- short to catch an error and throw a new one
throw :: Error -> (Error -> ErrorParser a)
throw = const . throwError

-- return parsed value, assuming at least one successful parse
parseE :: ErrorParser a -> String -> Either Error a
parseE m s | null correct  = fst $ head $ sortOn (length . snd) result
           | otherwise     = fst $ head $ correct
             where result  = f (runExceptT m) s
                   correct = filter (\(x,y) -> isRight x && null y) result

-- combine two given parsers
combine :: ErrorParser a -> ErrorParser a -> ErrorParser a
combine p1 p2 = ExceptT $ Parser $ \s -> f (runExceptT p1) s ++ f (runExceptT p2) s

-- match a character
charE :: ErrorParser Char
charE = do pc <- lift $ Parser f
           maybe (throwError CharError) return pc
        where f []    = [(Nothing,"")] 
              f (c:s) = [(Just c,s)]

-- match a character if it meets the given condition
spotE :: (Char -> Bool) -> ErrorParser Char
spotE p = do c <- charE `catchError` (throw SpotError)
             if p c then return c else throwError SpotError

-- match a specific character
tokenE :: Char -> ErrorParser Char
tokenE c = spotE (==c) `catchError` (throw $ TokenError c)

-- match a specific string
matchE :: String -> ErrorParser String
matchE s = mapM tokenE s `catchError` (throw $ MatchError s) 

-- match zero or more occurrences
starE :: ErrorParser a -> ErrorParser [a]
starE p = combine (plusE p) (return [])

-- match one or more occurrences
plusE :: ErrorParser a -> ErrorParser [a]
plusE p = do x <- p
             xs <- starE p
             return (x:xs)

-- match a natural number
parseNatE :: ErrorParser Int
parseNatE = read <$> (plusE $ spotE isDigit)

-- match a negative number
parseNegE :: ErrorParser Int
parseNegE = do tokenE '-'
               n <- parseNatE
               return (-n)

-- match an integer
parseIntE :: ErrorParser Int
parseIntE = combine parseNatE parseNegE `catchError` (throw IntError)

-- match an occurence from one of the given parsers
parseOneOfE :: [ErrorParser a] -> ErrorParser a
parseOneOfE [x]    = x
parseOneOfE (x:xs) = combine x $ parseOneOfE xs 

-- match an end-of-line-character
parseEndE :: ErrorParser String
parseEndE = parseListElemE ["\r","\n","\r\n"] `catchError` (throw MissingEndError) 

-- match content between brackets
parseBracketsE :: ErrorParser a -> ErrorParser a
parseBracketsE p = do tokenE '(' `catchError` (throw BracketsError) 
                      x <- p
                      tokenE ')' `catchError` (throw BracketsError)
                      return x

-- match an item header
parseItemE :: ErrorParser a -> ErrorParser a
parseItemE p = do (tokenE '*' >> plusE (tokenE ' ')) `catchError` (throw ItemError)
                  x <- p
                  (tokenE ':' >> combine (plusE (tokenE ' ')) (matchE "\t")) `catchError` (throw ItemError)
                  return x

-- match one or more occurences separated by a delimiter
parseWithDelimE :: ErrorParser a -> ErrorParser b -> ErrorParser [a]
parseWithDelimE p1 p2 = do x <- p1
                           xs <- starE (p2 >> p1)
                           return (x:xs)

parseListElemE :: [String] -> ErrorParser String
parseListElemE [x]    = matchE x
parseListElemE (x:xs) = combine (matchE x) (parseListElemE xs)

----------------
-- GameParser --
----------------

-- match a line where a power is descibed
parsePowerE :: ErrorParser (Power,Int)
parsePowerE = do x <- parseItemE $ parseListElemE ["freeze","double tap"] `catchError` (throwError . InvalidPowerError)
                 i <- parseNatE `catchError` (throw InvalidChargeError)
                 (if i == 1 then matchE " charge" else matchE " charges") `catchError` (throw WrongConjugateError)
                 parseEndE
                 let result | i<1           = throwError InvalidChargeError
                            | x == "freeze" = return (Freeze,i) 
                            | otherwise     = return (DoubleTap,i)
                 result


-- matches multiple lines with a power
parsePowerLinesE :: ErrorParser Powers
parsePowerLinesE = M.fromList <$> (parseEndE >> plusE parsePowerE)

-- match the empty powers
parseNoneE :: ErrorParser Powers
parseNoneE = do matchE " None" `catchError` (throw NonePowerError)
                parseEndE >> return M.empty

-- match the power description block
parsePowersE :: ErrorParser Powers
parsePowersE = do matchE "powers:" `catchError` (throw PowerHeadError)
                  combine parseNoneE parsePowerLinesE

-- match a cell zone
parseZoneE :: ErrorParser [Coord]
parseZoneE = do z <- parseListElemE ["cell","line","hexagon","tilted parallelogram","backward parallelogram","forward parallelogram","tube"] `catchError` (throw InvalidZoneNameError)
                c <- parseBracketsE $ parseWithDelimE parseIntE (matchE ", ") `catchError` (throwError . InvalidZoneError)
                let result | z == "cell" && length c == 2                   = return [(c!!0,c!!1)]
                           | z == "line" && length c == 4                   = return $ lineFromCoord c
                           | z == "hexagon" && length c == 3                = return $ adjacentCoord (abs (c!!2)) [(c!!0,c!!1)] 
                           | z == "tilted parallelogram" && length c == 4   = return $ concat [lineFromCoord [c!!0+t,c!!1,c!!0-c!!2+t,c!!1+c!!2] | t <- [0..c!!3]]
                           | z == "backward parallelogram" && length c == 4 = return $ concat [lineFromCoord [c!!0+t,c!!1-t,c!!0+t,c!!1+c!!2-t] | t <- [0..c!!3]]
                           | z == "forward parallelogram" && length c == 4  = return $ concat [lineFromCoord [c!!0+t,c!!1,c!!0+t,c!!1-c!!2] | t <- [0..c!!3]]
                           | z == "tube" && length c == 5                   = return $ adjacentCoord (abs (c!!4)) $ lineFromCoord $ take 4 c
                           | otherwise                                      = throwError $ MismatchingZoneError z
                result

-- match a celltype an convert the given zone to the celltype
parseTypeE :: [Coord] -> ErrorParser [Cell]
parseTypeE c = do t <- parseListElemE ["normal","blocked","nonblockable","open","target","robot"] `catchError` (throw InvalidTypeError)
                  case t of 
                       "normal"       -> return $ map Unblocked c
                       "blocked"      -> return $ map Blocked c
                       "nonblockable" -> return $ map Nonblockable c
                       "open"         -> return $ map Open c
                       "target"       -> return $ map Target c
                       "robot"        -> do s <- parseBracketsE parseIntE `catchError` (throwError . InvalidRobotSpeedError)
                                            if s>0 then return (map (\x -> Robot x s False) c) else throwError ZeroRobotSpeedError

-- match a single gridline
parseGridLineE :: ErrorParser [Cell]
parseGridLineE = do c <- parseItemE parseZoneE
                    cells <- parseTypeE c
                    parseEndE >> return cells

-- match a grid desciption
parseGridE :: ErrorParser HgC
parseGridE = do matchE "cells:" `catchError` (const $ throwError CellHeadError)
                cells <- nubOrdOn getPos . concat . reverse <$> (parseEndE >> plusE parseGridLineE)
                return (Grid cells False)

-- match a Game
parseGameE :: ErrorParser (Game HgC)
parseGameE = do powers <- parsePowersE
                grid <- parseEndE >> parseGridE
                let firstcoord = getPos $ head $ cells grid
                return (Game grid (Cursor firstcoord) powers)

-- match the line between the descriptions of games
parseDashedE :: ErrorParser ()
parseDashedE = (matchE "---" >> starE (tokenE '-') >> parseEndE >> return ()) `catchError` (throw DashError)

-- match a levelfile
parseLevelsE :: ErrorParser [Game HgC]
parseLevelsE = parseWithDelimE parseGameE parseDashedE

