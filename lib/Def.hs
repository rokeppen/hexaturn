module Def where

import qualified Graphics.Gloss.Interface.Pure.Game as G
import           Data.Maybe (listToMaybe,isJust)
import           Data.Map (Map)

----------------
-- Data types --
----------------

type Coord = (Int,Int)

type GlossCoord = (Float,Float)

data Power = Freeze | DoubleTap deriving (Eq,Ord)

type Powers = Map Power Int

newtype Cursor = Cursor { curPos :: Coord }

data Cell = Unblocked { pos :: Coord } | Blocked { pos :: Coord } | Nonblockable { pos :: Coord } | Open { pos :: Coord } | Target { pos :: Coord } | Robot { pos :: Coord, speed :: Int, blocked :: Bool } deriving (Eq,Ord)

data HexaturnGrid c = Grid { cells :: [c], frozen :: Bool }

data Game g = Game { grid :: g, cursor :: Cursor, powers :: Powers }

type HgC = HexaturnGrid Cell 

data Frame = GameFrame (Game HgC) | SubFrame | EndFrame

------------------------
-- Class Positionable --
------------------------

-- class for data types that have a coordinate property
class Positionable p where
   getPos :: p -> Coord

-- Positionable instance for Cursor
instance Positionable Cursor where
   getPos = curPos

-- Positionable instance for Cell
instance Positionable Cell where
   getPos = pos

----------------------
-- Class Invertable --
----------------------

-- class for objects that have an invertable property
class Invertable c where
   invert :: c -> c

-- Invertable instance for Cell: only Blocked an Unblocked can be inverted to each other
instance Invertable Cell where
   invert (Unblocked c) = Blocked c
   invert (Blocked c)   = Unblocked c
   invert x             = x

-------------------------
-- Class Dimensionable --
-------------------------

-- class for objects with minimal and maximal coordinates
class Dimensionable d where
   getMin :: d -> Coord 
   getMax :: d -> Coord

-- Dimensionable instance for HexaturnGrid
instance Positionable a => Dimensionable (HexaturnGrid a) where
   getMin grid = (xmin,ymin)
                where xmin = minimum $ map (fst . getPos) $ cells grid
                      ymin = minimum $ map (snd . getPos) $ cells grid
   getMax grid = (xmax,ymax)
                where xmax = maximum $ map (fst . getPos) $ cells grid
                      ymax = maximum $ map (snd . getPos) $ cells grid

----------------
-- Class Grid --
----------------

-- class for objects that can be interpreted as a grid
class GridClass g where
   adjacent  :: (Positionable c) => g c -> Coord -> [Coord]
   getCell   :: (Positionable c) => g c -> Coord -> Maybe c
   transform :: g c -> (c -> Bool) -> (c -> c) -> g c

-- GridClass instance for HexaturnGrid
instance GridClass HexaturnGrid where
   adjacent grid (x,y)    = filter (isJust . getCell grid) [(x-1,y+1),(x+1,y-1),(x+1,y),(x-1,y),(x,y+1),(x,y-1)]
   getCell grid           = listToMaybe . get (cells grid)
   transform grid boolf f = fmap (\c -> if boolf c then f c else c) grid

-- returns the objects from a list of Positionables that have the given coordinate
get :: Positionable a => [a] -> Coord -> [a]
get list c = filter ((c==) . getPos) list

--------------------------
-- Existing typeclasses --
--------------------------

instance Foldable HexaturnGrid where
   foldr f acc = foldr f acc . cells

instance Functor HexaturnGrid where
   fmap f g = g { cells = map f (cells g) }

instance Functor Game where
   fmap f g = g { grid = f (grid g) }

