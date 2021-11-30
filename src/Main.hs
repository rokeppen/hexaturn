module Main where

import qualified Graphics.Gloss.Interface.IO.Game as G
import           Def
import           GameParser
import           ErrorParser
import           Methods
import           Render
import           System.Exit (exitSuccess)
import           System.Environment (getArgs)
import           System.IO (openFile,hGetContents,hClose,IOMode(ReadMode))
import           Data.Maybe (maybe)
import           Data.Either (either,fromLeft,isLeft)
import           Data.List (intersperse)
import           Util (fstOf3)
import           Control.Monad (when)

------------------------
-- GUI implementation --
------------------------

type GW = (Frame,[Frame],Int)

playGUI :: G.Display -> G.Color -> Int -> GW -> (GW -> G.Picture) -> (G.Key -> GW -> IO GW) -> IO ()
playGUI window color fps world dw he = G.playIO window color fps world dwIO heIO tsIO
      where dwIO                           = return . dw
            heIO (G.EventKey s G.Down _ _) = he s
            heIO _                         = return 
            tsIO _                         = return

-- handles key inputs on a game:
-- * q results always in quitting the game
-- * entering proceeds to the next level when showing a SubFrame
-- * keys on an ongoing level:
--   ** the level restarts on r or when the level is lost after a move
--   ** the game proceeds to a SubFrame when a move results in winning the level, otherwise the current level is altered
--   ** when no move is made, the handling shifts to keyHandlerOnGame
-- * in all other situations (eg. on an EndFrame,...), a pressed key does nothing
keyHandler :: G.Key -> GW -> IO GW
keyHandler (G.Char 'q') _                                = exitSuccess
keyHandler (G.SpecialKey G.KeyEnter) w@(SubFrame,l,i)    = return (l!!(i+1),l,i+1)
keyHandler (G.SpecialKey G.KeySpace) w@(GameFrame g,l,i) | isLost newgame = return (l!!i,l,i)
                                                         | isWon newgame  = return (l!!(i+1),l,i+1)
                                                         | otherwise      = return (GameFrame newgame,l,i)
                                                         where coord   = getPos $ cursor g
                                                               subgame = tapCell g coord 
                                                               newgame | isTappable (grid g) coord && not (frozen (grid g)) = moveRobots subgame
                                                                       | otherwise                                          = subgame
keyHandler (G.Char 'r') (GameFrame _,l,i)                = return (l!!i,l,i)
keyHandler (G.Char s) w@(GameFrame g,l,i)                = return (GameFrame $ keyHandler' s g,l,i)
keyHandler _ w                                           = return w

-- handles key inputs on a level:
-- * f/c activate their corresponding power
-- * w/e/d/x/z change the cursor position
-- * every other key does nothing
keyHandler' :: Char -> Game HgC -> Game HgC
keyHandler' 'f' = usePower Freeze
keyHandler' 'c' = usePower DoubleTap
keyHandler' 'w' = changeFocus (-1,0)
keyHandler' 'e' = changeFocus (-1,1)
keyHandler' 'd' = changeFocus (0,1)
keyHandler' 'x' = changeFocus (1,0)
keyHandler' 'z' = changeFocus (1,-1)
keyHandler' 'a' = changeFocus (0,-1)
keyHandler' _   = id

-- changes focus of the cursor, based on a difference in coordinates
changeFocus :: Coord -> Game HgC -> Game HgC
changeFocus (difx,dify) (Game g cur p) = Game g newCur p
                                         where (x,y)  = getPos cur
                                               newPos = (x+difx,y+dify)
                                               newCur = maybe cur (Cursor . getPos) $ getCell g newPos   

------------------------
-- CLI implementation --
------------------------

type CW = (Game HgC,[Game HgC],Int)

playCLI :: CW -> (CW -> String) -> (CW -> (Command,Coord) -> CW) -> IO ()
playCLI w@(g,l,i) strRep handler | isWon g && i+1 == length l = return ()
                                 | isWon g                    = playCLI (l!!(i+1),l,i+1) strRep handler
                                 | otherwise                  = do putStrLn $ strRep w
                                                                   input <- getLine
                                                                   let cmd = parse (parseCommand g) input
                                                                       nw  = either (const w) (handler w) cmd
                                                                   when (isLeft cmd) $ putStrLn $ input ++ " is geen geldige input."
                                                                   playCLI nw strRep handler

-- handles valid CLI commands:
-- * if the level is lost after executing the command, restart the current level
-- * otherwise, just execute the command
comHandler :: CW -> (Command,Coord) -> CW
comHandler w@(g,l,i) c | isLost newgame = (l!!i,l,i)
                       | otherwise      = (newgame,l,i)
                       where handle Click  = moveRobots . tapCell g
                             handle Frozen = tapCell (usePower Freeze g)
                             handle Double = moveRobots . tapCell (usePower DoubleTap g)
                             newgame       = uncurry handle c

----------
-- Main --
----------

-- sets window
window :: G.Display
window = G.InWindow "Hexaturn" (500,500) (200,200)

main :: IO ()
main = do args <- getArgs
          handle <- openFile (args!!1) ReadMode
          contents <- hGetContents handle
          let parsed = parseE parseLevelsE contents
              levels = either (const []) id parsed
          if isLeft parsed
             then do let error = fromLeft CharError parsed
                     putStrLn $ "Could not parse levelfile: " ++ show error
             else if head args == "GUI"
                     then do let frames = intersperse SubFrame (map GameFrame levels) ++ [EndFrame]
                             playGUI window (G.greyN 0.8) 3 (head frames,frames,0) (draw . fstOf3) keyHandler 
                     else do let shiftedLevels = map (fmap shift) levels 
                             playCLI (head shiftedLevels,shiftedLevels,0) (show . fstOf3) comHandler
          hClose handle

