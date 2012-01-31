{-
    Zachary Weaver <zaw6@pitt.edu>
    Version 0.1.1
    Main.hs
    
    This is the actual game code
-}

import Control.Monad (replicateM_, forM_)
import Control.Monad.Cont
import Control.Monad.State.Strict
import System.Random (randomRIO)

import Data.IORef
import Data.Lens.Common
import qualified Data.Map as Map

import qualified Graphics.UI.SDL as SDL
import Graphics.UI.SDL.Keysym
import qualified Graphics.UI.SDL.Image as SDLi
import qualified Graphics.UI.SDL.TTF as SDLt

import GameSpace
import Util
import Plane

type SpriteMap = Map.Map String SDL.Surface
type FontMap   = Map.Map String SDLt.Font

-- | The rate of movement in pix / ms
rate :: Double
rate = 1 / 50

-- | The size of the header info bar
header :: Int
header = 10

-- | The resource directory
resDir :: String -> String
resDir = ("res/"++)

main = do
    putStr "Initializing SDL ..."
    SDL.init [SDL.InitVideo, SDL.InitEventthread, SDL.InitTimer]
    SDLt.init
    SDL.setVideoMode 0 0 32 
        [ SDL.DoubleBuf
        , SDL.HWSurface
        , SDL.Fullscreen
        ]
    SDL.setCaption "Hobgoblin" "hobgoblin"
    putStrLn " done."
    startGame
    putStrLn "Thanks for playing!"

loadSprites :: IO SpriteMap
loadSprites = flip execStateT Map.empty $ do
    insertS "Man" "Man_20x40.png"
    insertS "ManWithShield" "ManWithShield_20x40.png"
    insertS "Hobgoblin" "Hobgoblin_20x40.png"
    insertS "Boot" "Boot_7x7.png"
    insertS "HobSkull" "HobSkull_7x7.png"
    insertS "Coin" "Coin_7x7.png"
  where insertS :: String -> String -> StateT SpriteMap IO ()
        insertS name file = do
            sprite <- lift $ SDLi.load $ resDir file
            modify (Map.insert name $ sprite)       

loadFonts :: IO FontMap
loadFonts = flip execStateT Map.empty $ do
    insertF "Georgia7" "georgia.ttf" 7
    insertF "Georgia24" "georgia.ttf" 24
  where insertF :: String -> String -> Int -> StateT FontMap IO ()
        insertF name file point = do
            font <- lift $ SDLt.openFont (resDir file) point
            modify (Map.insert name $ font)

   
startGame :: IO ()
startGame = do
    putStr "Loading sprites ..."
    sprites <- loadSprites
    fonts <- loadFonts
    putStrLn " done."
    putStrLn "Starting new game"
    execStateT (playGame sprites fonts) newGame
    putStrLn "Game Over"
    promptNewGame >>= (startGame ?? return ())

promptNewGame = return False

pollEvents :: IO [SDL.Event]
pollEvents = do
    event <- SDL.pollEvent
    return [] ?? fmap (event:) pollEvents $ event == SDL.NoEvent

getScreenSize :: IO Size
getScreenSize = do
    screen <- SDL.getVideoSurface
    let w = SDL.surfaceGetWidth screen
    let h = SDL.surfaceGetHeight screen
    return (w, h)

-- | Draw the initial Boxes for the Header
initHeader :: SpriteMap -> Int -> GameState IO ()
initHeader sprites w = do
    screen <- lift SDL.getVideoSurface
    let pxlfmt = SDL.surfaceGetPixelFormat screen
    blue <- lift $ SDL.mapRGB pxlfmt 0 0 255
    green <- lift $ SDL.mapRGB pxlfmt 0 255 0
    white <- lift $ SDL.mapRGB pxlfmt 255 255 255
    let Just boot = Map.lookup "Boot" sprites
    let Just hobSkull = Map.lookup "HobSkull" sprites
    let Just coin = Map.lookup "Coin" sprites
    lift $ SDL.fillRect screen (Just $ SDL.Rect 1 1 102 7) blue
    lift $ SDL.fillRect screen (Just $ SDL.Rect 105 1 102 7) green
    lift $ SDL.fillRect screen (Just $ SDL.Rect 0 9 w 1) white
    lift $ SDL.blitSurface hobSkull Nothing screen $ Just $
        SDL.Rect 209 1 7 7
    return ()

playGame :: SpriteMap -> FontMap -> GameState IO ()
playGame sprites fonts = do
    -- We place the character in the middle and add an initial monster
    (w, h) <- lift getScreenSize
    setStateL gsLocation 
        (fromIntegral w / 2 - 10, 
        (fromIntegral h + fromIntegral header) / 2 - 20)
    genGoblin
    initHeader sprites w
    -- We seed the game loop with 1 frame having passed
    gameLoop (1 / rate) sprites fonts
    -- Add score reporting and high scores here
    kills <- getStateL gsTotalKills
    score <- getStateL gsScore
    when (kills > 0) $ lift $ do
        putStr "Total Kills: "
        print kills
        putStr "Score: "
        print score
        putStr "Skill Rating: "
        print $ score `div` kills

-- | Generates a random monster that does not hit the player
genGoblin :: GameState IO ()
genGoblin = do 
    (w, h) <- lift getScreenSize
    (px, py) <- getStateL gsLocation
    gx <- fmap (avoid px) $ lift $ randomRIO (0, fromIntegral w - 40)
    gy <- fmap (avoid py . (+fromIntegral header)) $ lift $ randomRIO
        (0, fromIntegral h - 40 - fromIntegral header)
    modStateL gsGoblins ((gx, gy):)
  where avoid obs val = ((val + 20) ?? val) $ 0 <= delta && delta < 20
          where delta = val - obs

gameLoop delta sprites fonts = flip runContT return $ callCC $ \exit -> do
    actions <- handleEvents exit
    startTime <- lift2 SDL.getTicks
    lift $ do 
        clearScreen
        mKills <- gsUpdate (delta * rate) actions
        gsDrawSpace
        lift $ SDL.getVideoSurface >>= SDL.flip
        elapTime <- fmap (fromIntegral . subtract startTime) 
            $ lift SDL.getTicks
        case mKills of
            Nothing -> return ()
            Just kills -> do
                replicateM_ (kills * 2) genGoblin
                gameLoop elapTime sprites fonts
  where clearScreen :: GameState IO ()
        clearScreen = do
            -- Prepare resources for drawing
            screen <- lift SDL.getVideoSurface
            let pxlfmt = SDL.surfaceGetPixelFormat screen
            black <- lift $ SDL.mapRGB pxlfmt 0 0 0
            -- Clear header
            lift $ SDL.fillRect screen (Just $ SDL.Rect 2 2 100 5) black
            lift $ SDL.fillRect screen (Just $ SDL.Rect 106 2 100 5) black
            -- TODO: clear text
            -- Clear goblins
            goblins <- getStateL gsGoblins
            forM_ goblins $ lift . flip (SDL.fillRect screen) black 
                . Just . flip pointSize2Rect (20, 40)
            -- Clear Man
            loc <- getStateL gsLocation
            lift $ SDL.fillRect screen (Just $ pointSize2Rect loc (20, 40))
                black
            return ()
        gsDrawSpace :: GameState IO ()
        gsDrawSpace = do
            -- Prepare resources for drawing
            screen <- lift SDL.getVideoSurface
            let pxlfmt = SDL.surfaceGetPixelFormat screen
            lgreen <- lift $ SDL.mapRGB pxlfmt 127 255 127
            lblue  <- lift $ SDL.mapRGB pxlfmt 127 127 255
            -- Draw header
            power <- getStateL gsPower
            stamina <- getStateL gsStamina
            when (power >= 1) $ do
                lift $ SDL.fillRect screen 
                    (Just $ SDL.Rect 2 2 (floor power) 5) lblue
                return ()
            when (stamina >= 1) $ do 
                lift $ SDL.fillRect screen 
                    (Just $ SDL.Rect 106 2 (floor stamina) 5) lgreen
                return ()
            drawHeaderText
            -- Draw goblins
            goblins <- getStateL gsGoblins
            let (Just gobSurf) = Map.lookup "Hobgoblin" sprites
            forM_ goblins $ lift . SDL.blitSurface gobSurf Nothing screen
                . Just . flip pointSize2Rect (20, 40)
            -- Draw Man
            shield <- getStateL gsShield
            loc <- getStateL gsLocation
            let (Just manSurf) = flip Map.lookup sprites $ 
                    ("ManWithShield" ?? "Man") $ shield
            lift $ SDL.blitSurface manSurf Nothing screen $ Just $ 
                pointSize2Rect loc (20, 40)
            return ()
        drawHeaderText :: GameState IO ()
        drawHeaderText = do
            -- TODO: Write me
            return ()
        -- Holy shit, Dat type!
        handleEvents :: (() -> ContT () (StateT GameSpace IO) ()) ->
            ContT () (StateT GameSpace IO) [Action]
        handleEvents exit = do
            incActsRef <- lift2 $ newIORef (id)
            otherActsRef <- lift2 $ newIORef (id)
            events <- lift2 $ pollEvents
            forM_ events $ \event -> do
              case event of
                -- Exiting and other events
                SDL.Quit -> exit ()
                SDL.KeyDown Keysym{symKey=SDLK_ESCAPE} -> exit ()
                SDL.VideoResize w _ -> do
                    lift $ initHeader sprites w
                    lift $ gsDrawSpace
                -- Do Pause
                SDL.KeyDown Keysym{symKey=SDLK_p} -> do
                    screen <- lift2 $ SDL.getVideoSurface
                    let pxlfmt = SDL.surfaceGetPixelFormat screen
                    black <- lift2 $ SDL.mapRGB pxlfmt 0 0 0
                    let Just font = Map.lookup "Georgia24" fonts
                    fntSurf <- lift2 $ SDLt.renderTextSolid font "Pause"
                        $ SDL.Color 255 255 255
                    rect <- lift2 $ blitCenter fntSurf
                    lift2 $ SDL.flip screen
                    keyDelta <- lift2 $ pause fonts
                    modStateL gsKeysDown (+keyDelta)
                    lift2 $ SDL.fillRect screen (Just rect) black
                    return ()
                -- Pace Control
                SDL.KeyDown Keysym{symKey=SDLK_q} -> 
                    addAction incActsRef $ IncPace 1
                SDL.KeyDown Keysym{symKey=SDLK_e} -> 
                    addAction incActsRef $ IncPace (-1)
                -- Shield
                SDL.KeyDown Keysym{symKey=SDLK_SPACE} -> 
                    addAction otherActsRef ToggleShield
                -- Movement
                SDL.KeyDown Keysym{symKey=SDLK_w} -> do
                    modStateL gsKeysDown (+1)
                    addAction otherActsRef $ Move $ Just North
                SDL.KeyDown Keysym{symKey=SDLK_s} -> do
                    modStateL gsKeysDown (+1)
                    addAction otherActsRef $ Move $ Just South
                SDL.KeyDown Keysym{symKey=SDLK_a} -> do
                    modStateL gsKeysDown (+1)
                    addAction otherActsRef $ Move $ Just West
                SDL.KeyDown Keysym{symKey=SDLK_d} -> do
                    modStateL gsKeysDown (+1)
                    addAction otherActsRef $ Move $ Just East
                SDL.KeyUp Keysym{symKey=key} -> 
                    when (key `elem` [SDLK_w,SDLK_s,SDLK_a,SDLK_d]) $ do
                        modStateL gsKeysDown $ subtract 1
                        kdown <- getStateL gsKeysDown
                        when (kdown <= 0) $
                            addAction otherActsRef $ Move Nothing
                _ -> return ()
            incActs <- lift2 $ readIORef incActsRef
            otherActs <- lift2 $ readIORef otherActsRef
            return $ (incActs . otherActs) []
          where addAction ioRef action =
                    lift2 $ modifyIORef ioRef (.(action:))

-- | Blits to center screen and returns dirty rectangle
blitCenter :: SDL.Surface -> IO SDL.Rect
blitCenter surf = do
    screen <- SDL.getVideoSurface
    (sw, sh) <- getScreenSize
    let pxlfmt = SDL.surfaceGetPixelFormat screen
    black <- SDL.mapRGB pxlfmt 0 0 0
    let tw = SDL.surfaceGetWidth surf
    let th = SDL.surfaceGetHeight surf
    let tRect = SDL.Rect (sw `div` 2 - tw `div` 2) 
                    ((sh - header) `div` 2 - th `div` 2 + header) tw th
    flip (SDL.fillRect screen) black $ Just $ SDL.Rect 
        (getL rectXL tRect - 1) (getL rectYL tRect - 1) (tw + 2) (th + 2)
    SDL.blitSurface surf Nothing screen $ Just tRect
    return tRect

pause :: FontMap -> IO Int
pause fonts = execStateT pauseLoop 0
  where pauseLoop :: StateT Int IO ()
        pauseLoop = do
            event <- lift SDL.pollEvent
            case event of
                SDL.Quit -> return ()
                SDL.KeyDown Keysym{symKey=SDLK_ESCAPE} -> return ()
                SDL.KeyDown Keysym{symKey=SDLK_p} -> return ()
                SDL.KeyDown Keysym{symKey=key} -> do
                    whenMoveKey key (+1)
                    pauseLoop
                SDL.KeyUp Keysym{symKey=key} -> do
                    whenMoveKey key (subtract 1)
                    pauseLoop
                _ -> pauseLoop
        whenMoveKey key inc =
            when (key `elem` [SDLK_w,SDLK_s,SDLK_a,SDLK_d]) $ modify inc
