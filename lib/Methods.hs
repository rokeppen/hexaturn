module Methods where

import Def
import Data.Map (Map,singleton,member,union,fromList,lookup,findWithDefault,adjust)
import Data.Maybe (listToMaybe,mapMaybe,maybe)
import Data.Sort (sortOn)
import Data.List.Extra (disjoint,delete,(\\),notNull)

-- applies the floodfill algorithm starting from a given coordinate and returns the shortest possible path to one of the given coordinates, if one exists  
shortestPath :: HgC -> Coord -> [Coord] -> Maybe [Coord]
shortestPath g start targets = listToMaybe paths
                               where floodres = flood [start] (singleton start start) g
                                     paths    = sortOn length $ map reverse $ mapMaybe (\t -> pathFromMap start t floodres) targets

-- floodfill alorithm
flood :: [Coord] -> Map Coord Coord -> HgC -> Map Coord Coord
flood [] x _       = x
flood front from g = let (cur:xs) = front
                         toLook   = filter (\c -> isWalkable g c && not (member c from)) (adjacent g cur)
                         newfront = xs ++ toLook 
                         newfrom  = union from (fromList [(i,cur) | i <- toLook])
                     in flood newfront newfrom g

-- recover a path from a floodfill-map
pathFromMap :: Coord -> Coord -> Map Coord Coord -> Maybe [Coord]
pathFromMap start dest map | start == dest = Just [start]
                           | otherwise     = do dest' <- Data.Map.lookup dest map
                                                rest <- pathFromMap start dest' map
                                                return $ dest : rest

-- getters for robots and targets
robots, targets :: HgC -> [Cell]
robots g  = [x | x@Robot {} <- cells g]
targets g = [x | x@Target {} <- cells g]

rCoord, tCoord :: HgC -> [Coord]
rCoord = map getPos . robots
tCoord = map getPos . targets

-- tries to move the robots on a grid:
-- * robots cannot move when the grid is frozen
-- * if the grid is not frozen, the floodfill algorithm is used to determine for each robot the shortest path to a target:
--   ** if none exist, the robot stays put
--   ** otherwise, move the robot the maximum number of steps along the path
moveRobots :: Game HgC -> Game HgC
moveRobots h@(Game (Grid _ True) _ _) = h
moveRobots h@(Game g@(Grid c f) _ _)  = h { grid = Grid newCells f }
                                        where newCoord (Robot rc s _)       = maybe rc (\path -> if length path <= s then last path else path !! s) $ shortestPath g rc $ tCoord g
                                              newCells                      = foldr mapper c $ robots g
                                              mapper r@(Robot rc _ b) cells | rc == newc = cells
                                                                            | b          = r { blocked = False } : withoutr
                                                                            | otherwise  = Unblocked rc : r { pos = newc } : withoutn
                                                                            where newc     = newCoord r
                                                                                  withoutr = delete r cells
                                                                                  withoutn = delete (Open newc) $ delete (Unblocked newc) withoutr

-- checks if a robot can jump to a given coordinate: existing, not blocked and not open (so robots can share a cell)
isWalkable :: HgC -> Coord -> Bool
isWalkable g c = maybe False walkable $ getCell g c
                 where walkable (Blocked _) = False
                       walkable (Open _)    = False
                       walkable _           = True

-- the game is won when no robot can reach a cell
isWon :: Game HgC -> Bool
isWon (Game g _ _) = null $ mapMaybe (\(Robot c _ _) -> shortestPath g c $ tCoord g) $ robots g 

-- the game is lost when a robot reaches a target
isLost :: Game HgC -> Bool
isLost (Game g _ _) = not $ disjoint (rCoord g) $ tCoord g

-- blocks the robots satisfying the predicate
blockRobots :: HgC -> (Cell -> Bool) -> HgC
blockRobots g f = g { cells = map (\x -> x { blocked = True }) block ++ noBlock }
                  where block   = filter f $ robots g
                        noBlock = cells g \\ block

-- handles tap on a tappable cell with given coordinate:
-- * if the grid is frozen, it blocks the robots on the given coordinate an unfreezes the grid
-- * if not, it inverts the cell on the given coordinate
tapCell :: Game HgC -> Coord -> Game HgC
tapCell (Game g x p) c | isTappable g c && frozen g = Game g { cells = cells $ blockRobots g ((c==) . getPos), frozen = False } x p
                       | isTappable g c             = Game (transform g ((c==) . getPos) invert) x p
                       | otherwise                  = Game g x p     

-- checks the charge of a power 
charge :: Power -> Powers -> Int
charge = findWithDefault 0

-- checks if a cell is tappable in a grid: 
-- * if the board is not frozen: existing, not Nonblockable, not a Robot and not a Target
-- * if the board is frozen: existing (implicit, if it is in the robotlist, it must be a cell too) and containing at least one unblocked robot
isTappable :: HgC -> Coord -> Bool
isTappable g c | frozen g    = notNull robotsOnCoord && not allRobotsBlocked 
               | otherwise   = maybe False blockable $ getCell g c
                   where robotsOnCoord           = get (robots g) c
                         allRobotsBlocked        = all blocked robotsOnCoord
                         blockable (Blocked _)   = True
                         blockable (Unblocked _) = True
                         blockable _             = False

-- tries to activate a power:
-- * check if the power has still charges left an can be activated:
--   ** Frozen: the power can only be activated when it is not yet active
--   ** DoubleTap: the power can only be activated when the board is not frozen, if it is used when it is already active, the second usage does nothing.
-- * apply the effect of the power:
--   ** Frozen: the board goes to a frozen state, so it knows a robot cell will be selected to block
--   ** DoubleTap: all the robot become blocked
-- * decrement the charge of the chosen power
usePower :: Power -> Game HgC -> Game HgC
usePower x (Game g@(Grid _ f) y p) | valid && x == Freeze = Game g { frozen = True } y (adjust (subtract 1) x p)
                                   | valid                = Game (blockRobots g (const True)) y (adjust (subtract 1) x p)
                                   | otherwise            = Game g y p
                                   where valid = charge x p > 0 && not f

-- shifts the cells from a grid until the minimal coordinates in both directions are zero
shift :: HgC -> HgC
shift g = g { cells = map shiftCell (cells g) }
          where shiftCell c = c { pos = ((fst $ getPos c)-xnew,(snd $ getPos c)-ynew) }
                (xnew,ynew) = getMin g

