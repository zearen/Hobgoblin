{-
    Zachary Weaver
    Version 0.1.1
    Rect.hs
    
    I thought I'd improve on the SDL a bit.  Provides utilities for
    working with rectanles and points
-}

module Plane where

import Data.Lens.Common
import Control.Monad.Cont

import Graphics.UI.SDL.Rect

type Point = (Double, Double)
type Size  = (Int, Int)

data Direction = North
               | South
               | East
               | West
    deriving (Eq, Ord, Show)

-- Create a lens interface for Rect
rectXL :: Lens Rect Int
rectXL = lens rectX (\x rect -> rect{rectX=x})
rectYL :: Lens Rect Int
rectYL = lens rectY (\y rect -> rect{rectY=y})
rectWL :: Lens Rect Int
rectWL = lens rectW (\w rect -> rect{rectW=w})
rectHL :: Lens Rect Int
rectHL = lens rectH (\h rect -> rect{rectH=h})

pointSize2Rect :: Point -> Size -> Rect
pointSize2Rect (x, y) (w, h) = Rect (floor x) (floor y) w h

rect2Point :: Rect -> Point
rect2Point (Rect x y _ _) = (fromIntegral x, fromIntegral y)

rect2Size :: Rect -> Size
rect2Size (Rect _ _ w h) = (w, h)

-- Create more lenses for Point and Size

rectPoint :: Lens Rect Point
rectPoint = lens rect2Point (\p rect -> pointSize2Rect p $ rect2Size rect)

rectSize :: Lens Rect Size
rectSize = lens rect2Size (\s rect -> pointSize2Rect (rect2Point rect) s)

-- | Increment a point by a given Direction
incPoint :: Direction -> Double -> Point -> Point
incPoint North inc (x, y) = (x, y-inc)
incPoint South inc (x, y) = (x, y+inc)
incPoint East inc (x, y) = (x+inc, y)
incPoint West inc (x, y) = (x-inc, y)

-- | Increment the point component of a Rect
incRect :: Direction -> Double -> Rect -> Rect
incRect dir inc = modL rectPoint $ incPoint dir inc

-- | Returns the second point moved one step closer to the first
movePointCloser :: Double -> Point -> Point -> Point
movePointCloser inc p1@(x1, y1) p2@(x2, y2) = flip runCont id $ do
    plens <- if distance x1 x2 >= distance y1 y2
               then return fstLens
               else return sndLens
    modifier <- callCC $ \ret -> do
        when (getL plens p1 < getL plens p2) (ret $ subtract inc)
        when (getL plens p1 > getL plens p2) (ret (+inc))
        return id
    return $ modL plens modifier p2
  where distance a b = abs $ a - b

-- Yes, yes, we could just use rect_ here, but I made these lenses
-- + I'm going to use them Gorram it!

-- | True if the first Rect completely contains the second
rectContains :: Rect -> Rect -> Bool
rectContains rect1 rect2 = flip runCont id $ callCC $ \exit -> do
    when (getL rectXL rect1 > getL rectXL rect2) (exit False)
    when (getL rectYL rect1 > getL rectYL rect2) (exit False)
    when (getL rectXL rect1 + getL rectWL rect1
        < getL rectXL rect2 + getL rectWL rect2) (exit False)
    when (getL rectYL rect1 + getL rectHL rect1
        < getL rectYL rect2 + getL rectHL rect2) (exit False)
    return True

-- | True if the two Rects intercect
rectIntercect :: Rect -> Rect -> Bool
rectIntercect rect1 rect2 = flip runCont id $ callCC $ \exit -> do
    checkOutside rectXL rectWL rect1 rect2 exit
    checkOutside rectXL rectWL rect2 rect1 exit
    checkOutside rectYL rectHL rect1 rect2 exit
    checkOutside rectYL rectHL rect2 rect1 exit
    return True
  where checkOutside locLens lenLens rect1 rect2 exit =
            when (getL locLens rect1 + getL lenLens rect1
                < getL locLens rect2) (exit False)
            
