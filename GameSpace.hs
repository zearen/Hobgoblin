{-
    Zachary Weaver <zaw6@pitt.edu>
    GameSpace.hs
    Version 0.1.1
    
    The game space
    
    Conventions:
        "inc" refers to incrementation
-}

{-# LANGUAGE TemplateHaskell #-}

module GameSpace where

import Data.List (partition)
import Control.Monad.State.Strict

import Data.Lens.Common
import Data.Lens.Template

import Plane
import Util

-- | A state monad transformer for GameSpace
type GameState m a = StateT GameSpace m a

-- | Relevant data to the gamespace
data GameSpace = GameSpace
        { _gsShield     :: Bool
        , _gsPower      :: Double
        , _gsPace       :: Int
        , _gsStamina    :: Double
        , _gsTotalKills :: Int
        , _gsKillCount  :: Int
        , _gsScore      :: Int
        , _gsKeysDown   :: Int
        , _gsMove       :: Maybe Direction
        , _gsLocation   :: Point
        , _gsGoblins    :: [Point]
        }

-- | Set defaults for new game
newGame :: GameSpace
newGame = GameSpace
        { _gsShield     = False
        , _gsPower      = 100
        , _gsPace       = 1
        , _gsStamina    = 100
        , _gsTotalKills = 0
        , _gsKillCount  = 0
        , _gsScore      = 0
        , _gsKeysDown   = 0
        , _gsMove       = Nothing
        , _gsLocation   = (0,0)
        , _gsGoblins    = []
        }

$(makeLens ''GameSpace)

-- | Possible actions by the user
data Action = Move (Maybe Direction)
            | ToggleShield
            | IncPace Int

-- | Update the gamespace.  Call this every cycle.
-- It's important that the list of actions contains
-- pace increments first.
-- It returns Nothing if 
gsUpdate :: Monad m => Double -> [Action] -> GameState m (Maybe Int)
gsUpdate delta acts = do
    gsMoveGoblins delta
    getStateL gsStamina >>=
        flip when (modStateL gsStamina (+delta)) . (<100)
    -- Check if we need to move
    getStateL gsMove >>= maybe (return ()) (\dir -> do
        pace <- getStateL gsPace
        stamina <- getStateL gsStamina
        pace <- if pace <= floor stamina
                  then return pace
                  else do
                    setStateL gsPace 1
                    return 1
        modStateL gsStamina $ subtract $ fromIntegral pace * delta / 2
        modStateL gsLocation $ incPoint dir $ fromIntegral pace * delta
        )
    mapM_ (gsDoAction delta) acts
    shield <- handleShield
    (hit, missed) <- gsPartitionGoblins
    if shield
      then do -- Kill the monsters and score points
        beforeKC <- getStateL gsKillCount
        forM_ hit $ \_ -> do
            modStateL gsTotalKills (+1)
            modStateL gsKillCount (+1)
            kc <- getStateL gsKillCount
            power <- getStateL gsPower
            stamina <- getStateL gsStamina
            modStateL gsScore (+(kc * 10 + 
                (100 - floor power) + (100 - floor stamina)))
        afterKC <- getStateL gsKillCount
        setStateL gsGoblins missed
        return $ Just $ afterKC - beforeKC
      else do -- Check to see if the player is dead
        return $ Just 0 ?? Nothing $ null hit
        -- Determines whether the shield is up
  where handleShield = do
            shield <- getStateL gsShield
            modStateL gsPower $ (subtract delta ?? (+delta)) shield
            power <- getStateL gsPower
            if shield
              then when (power < 0) $ setStateL gsShield False
              else when (power > 100) $ setStateL gsPower 100
            getStateL gsShield

-- | Performs the specified game action
gsDoAction :: Monad m => Double -> Action -> GameState m ()
gsDoAction delta (Move mdir) =
    setStateL gsMove mdir
gsDoAction _ ToggleShield = do
    getStateL gsShield >>= flip when (setStateL gsKillCount 0)
    modStateL gsShield not
gsDoAction _ (IncPace inc) = do 
    modStateL gsPace (+inc)
    pace <- getStateL gsPace
    when (pace < 1) $ setStateL gsPace 1
    when (pace > 100) $ setStateL gsPace 100

-- | Moves goblins closer to the player
gsMoveGoblins :: Monad m => Double -> GameState m ()
gsMoveGoblins delta = do
    loc <- getStateL gsLocation
    modStateL gsGoblins $ map $ movePointCloser delta loc

-- | Partitions goblins depending on whether they hit the player
-- (hit goblins, missed goblins)
gsPartitionGoblins :: Monad m => GameState m ([Point], [Point])
gsPartitionGoblins = do
    loc <- getStateL gsLocation
    goblins <- getStateL gsGoblins
    return $ partition (hitGoblin loc) goblins
  where hitGoblin loc monster = pointSize2Rect loc (20, 40)
            `rectIntercect` pointSize2Rect monster (20, 40)

