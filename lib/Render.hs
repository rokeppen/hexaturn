module Render (draw, show) where

import qualified Graphics.Gloss.Interface.Pure.Game as G
import           Def
import qualified Data.Map                           as M
import           Data.List (intercalate)

----------------------------
-- Class Drawable for GUI --
----------------------------

-- class for data types that can be translated in a Gloss image
class Drawable d where
   draw :: d -> G.Picture

-- Drawable instance for Cell
instance Drawable Cell where
   draw (Unblocked c)    = basicCell (G.greyN 0.7) c
   draw (Blocked c)      = basicCell (G.greyN 0.5) c
   draw (Nonblockable c) = basicCell G.white c
   draw (Open c)         = G.Blank
   draw (Target c)       = G.Pictures [draw (Nonblockable c), targetImg c]
   draw (Robot c s _)    = G.Pictures [draw (Nonblockable c), robotImg s c]

-- constants for drawing a cell
hexRadius, height, width, hexSide :: Float
hexRadius = 25 
height    = 2*hexRadius
width     = 2*hexRadius*sin(pi/3)
hexSide   = 2*hexRadius*cos(pi/3)

-- generate a cell image with a given inner color on a given position
basicCell :: G.Color -> Coord -> G.Picture
basicCell color c = uncurry G.Translate (hexToNormalCoord c) hex
                  where path   = map gonC [0..5]
                        gonC a = (hexRadius*cos((2*a+1)*pi/6),hexRadius*sin((2*a+1)*pi/6))        
                        subhex = G.Color color $ G.Polygon path
                        hex    = G.Pictures [subhex, G.lineLoop path] 

-- converts hexagonal coordinates to normal coordinates 
hexToNormalCoord :: Coord -> GlossCoord
hexToNormalCoord (x,y) = ((xn/2+yn)*width,-xn*(hexSide/2+height/2))
                         where (xn,yn) = (fromIntegral x,fromIntegral y)

-- draws a robot on a given coordinate: a rectangle with the width relating to the speed
robotImg :: Int -> Coord -> G.Picture
robotImg s c = uncurry G.Translate (hexToNormalCoord c) $ G.rectangleSolid (fromIntegral (5*s)) 10

-- draws a target on a given coordinate: a rhombus
targetImg :: Coord -> G.Picture
targetImg c = uncurry G.Translate (hexToNormalCoord c) $ G.Polygon [(5,0),(0,5),(-5,0),(0,-5)]

-- Drawable instance for Cursor
instance Drawable Cursor where
   draw (Cursor c) = uncurry G.Translate (hexToNormalCoord c) $ G.circleSolid 10

-- Drawable instance for HexaturnGrid
instance (Drawable a) => Drawable (HexaturnGrid a) where
   draw (Grid c _) = G.Pictures $ map draw c

-- Drawable instance for Game
instance (Drawable a, Dimensionable a) => Drawable (Game a) where
   draw (Game grid c p) = G.Pictures [draw grid,draw c,powerTile]
                          where powerTile = G.Translate 0 ytnew $ centerScaleText 0.2 $ printMap p
                                ytnew     = -(fromIntegral (fst $ getMax grid)+1)*height

-- Drawable instance for Frame
instance Drawable Frame where
   draw (GameFrame g) = draw g
   draw SubFrame      = G.Pictures [message,key1,key2]
                        where message = centerScaleText 0.4 "Well done!"
                              key1    = G.Translate 0 (-30) $ centerScaleText 0.2 "Press enter to continue"
                              key2    = G.Translate 0 (-55) $ centerScaleText 0.2 "or q to quit."
   draw EndFrame      = G.Pictures [message,key]
                        where message = centerScaleText 0.5 "Congratulations!"
                              key     = G.Translate 0 (-40) $ centerScaleText 0.2 "Press q to quit."

-- transforms a string in a gloss image, scales it with the given parameter and centers it (approximately)
centerScaleText :: Float -> String -> G.Picture
centerScaleText i s = G.Translate xnew 0 $ G.Scale i i $ G.Text s
                      where xnew     = -(numChars/2)*57*i -- 57 is an estimate for the mean character width in Gloss Text
                            numChars = fromIntegral $ length s

------------------------
-- Class Show for CLI --
------------------------

-- Show instance for Power
instance Show Power where
   show Freeze    = "freeze"
   show DoubleTap = "double tap"

-- custom string version for the Power map
printMap :: Powers -> String
printMap m | M.null m  = ""
           | otherwise = "(" ++ intercalate ", " (M.foldrWithKey (\p i acc -> (show p ++ ": " ++ show i) : acc) [] m) ++ ")" ++ nl

-- Show instance for Cell
instance Show Cell where
   show (Unblocked _)    = "O"
   show (Blocked _)      = "B"
   show (Nonblockable _) = "X"
   show (Open _)         = " "
   show (Target _)       = "T"
   show (Robot _ s _ )   = show s  

-- Show instance for HexaturnGrid
instance (Show a, Positionable a) => Show (HexaturnGrid a) where
   show g = header ++ chars
            where header         = leadtab ++ intercalate tab (map show [ymin..ymax]) ++ nl
                  (xmin,ymin)    = getMin g
                  (xmax,ymax)    = getMax g
                  getUpperChar c = maybe " " show $ getCell g c
                  line x         = [getUpperChar (x,y) | y <- [ymin..ymax]]
                  chars          = concat [show x ++ concat (replicate (x+1) leadtab) ++ intercalate tab (line x) ++ nl | x <- [xmin..xmax]]

-- constants for printing: the tabbing before the start of a cell row, the tabbing between cells and the EOL character
leadtab, tab, nl :: String
leadtab = "  "
tab     = "   "
nl      = "\n"

-- Show instance for Game
instance (Show a, Dimensionable a) => Show (Game a) where
   show (Game grid _ p) = printMap p ++ show grid

