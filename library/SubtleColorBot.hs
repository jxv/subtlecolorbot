module SubtleColorBot
  ( main
  ) where

import qualified Data.Text as T
import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import Control.Monad.Random
import Codec.Picture
import Numeric
import Web.Twitter.Conduit
import Network.HTTP.Client (RequestBody(..))

import SubtleColorBot.Env

-- Width enough using golden ratio
width, height :: Num a => a
width = 512
height = 316

lerp :: Float -> Float -> Float -> Float
lerp percent from to = percent * from + (1 - percent) * to

generatePercentages :: IO (Float, Float, Float)
generatePercentages = do
  x <- randomRIO (1, 100)
  y <- randomRIO (1, 100)
  z <- randomRIO (1, 100)
  let total = x + y + z
  return (x / total, y / total, z / total)

fluctuateInRange :: Pixel8 -> Float -> IO Pixel8
fluctuateInRange color' diff = do
  let color = fromIntegral color'
  let color0 = color - diff
  let color1 = color + diff
  let inRange = filter (\c -> c <= 255 && c >= 0) [color0, color1]
  offset <- getRandom
  let index = offset `mod` length inRange
  return $ pixel8 $ inRange !! index

pixel8 :: Float -> Pixel8
pixel8 = truncate

topToBottom :: (Pixel8, Pixel8) -> (Pixel8, Pixel8) -> (Pixel8, Pixel8) -> Int -> Int -> PixelRGB8
topToBottom (r, r') (g, g') (b, b') _x y = let
  rf = fromIntegral r
  gf = fromIntegral g
  bf = fromIntegral b
  rf' = fromIntegral r'
  gf' = fromIntegral g'
  bf' = fromIntegral b'
  p = fromIntegral y / height
  red = pixel8 $ lerp p rf rf'
  green = pixel8 $ lerp p gf gf'
  blue = pixel8 $ lerp p bf bf'
  in PixelRGB8 red green blue

rightToLeft :: (Pixel8, Pixel8) -> (Pixel8, Pixel8) -> (Pixel8, Pixel8) -> Int -> Int -> PixelRGB8
rightToLeft (r, r') (g, g') (b, b') x _y = let
  rf = fromIntegral r
  gf = fromIntegral g
  bf = fromIntegral b
  rf' = fromIntegral r'
  gf' = fromIntegral g'
  bf' = fromIntegral b'
  p = fromIntegral x / width
  red = pixel8 $ lerp p rf rf'
  green = pixel8 $ lerp p gf gf'
  blue = pixel8 $ lerp p bf bf'
  in PixelRGB8 red green blue

topLeftToBottomRight :: (Pixel8, Pixel8) -> (Pixel8, Pixel8) -> (Pixel8, Pixel8) -> Int -> Int -> PixelRGB8
topLeftToBottomRight (r, r') (g, g') (b, b') x y = let
  rf = fromIntegral r
  gf = fromIntegral g
  bf = fromIntegral b
  rf' = fromIntegral r'
  gf' = fromIntegral g'
  bf' = fromIntegral b'
  p = fromIntegral (x + y) / (width + height)
  red = pixel8 $ lerp p rf rf'
  green = pixel8 $ lerp p gf gf'
  blue = pixel8 $ lerp p bf bf'
  in PixelRGB8 red green blue

topRightToBottomLeft :: (Pixel8, Pixel8) -> (Pixel8, Pixel8) -> (Pixel8, Pixel8) -> Int -> Int -> PixelRGB8
topRightToBottomLeft (r, r') (g, g') (b, b') x y = let
  rf = fromIntegral r
  gf = fromIntegral g
  bf = fromIntegral b
  rf' = fromIntegral r'
  gf' = fromIntegral g'
  bf' = fromIntegral b'
  p = fromIntegral ((width - x) + y) / (width + height)
  red = pixel8 $ lerp p rf rf'
  green = pixel8 $ lerp p gf gf'
  blue = pixel8 $ lerp p bf bf'
  in PixelRGB8 red green blue

generators :: PixelRGB8 -> PixelRGB8 -> [Int -> Int -> PixelRGB8]
generators (PixelRGB8 r g b) (PixelRGB8 r' g' b') = (\f -> f (r, r') (g, g') (b, b')) <$> [topToBottom, rightToLeft, topLeftToBottomRight, topRightToBottomLeft]

randomGenerator :: PixelRGB8 -> PixelRGB8 -> IO (Int -> Int -> PixelRGB8)
randomGenerator color0 color1 = do
  index <- getRandom
  let gens = generators color0 color1
  return $ gens !! (index `mod` length gens)

hexColor :: PixelRGB8 -> String
hexColor (PixelRGB8 r g b) = "#" ++ showHex r "" ++ showHex g "" ++ showHex b ""

hexColorStatus :: PixelRGB8 -> PixelRGB8 -> String
hexColorStatus color0 color1 = hexColor color0 ++ " / " ++ hexColor color1

subtleColors :: Int -> IO (PixelRGB8, PixelRGB8)
subtleColors cap' = do
  let cap = fromIntegral cap'
  r <- getRandom :: IO Pixel8
  g <- getRandom :: IO Pixel8
  b <- getRandom :: IO Pixel8
  (dr, dg, db) <- generatePercentages
  r' <- fluctuateInRange r (dr * cap)
  g' <- fluctuateInRange g (dg * cap)
  b' <- fluctuateInRange b (db * cap)
  return (PixelRGB8 r g b, PixelRGB8 r' g' b')

delayMinutes :: Int -> IO ()
delayMinutes mins = delaySeconds (60 * mins)

delaySeconds :: Int -> IO ()
delaySeconds secs = threadDelay (1000000 * secs)

main :: IO ()
main = do
  twInfo <- getTWInfoFromEnv
  mgr <- newManager tlsManagerSettings
  forever $ do
    (color0, color1) <- subtleColors 100
    let status = hexColorStatus color0 color1
    generator <- randomGenerator color0 color1
    let image = generateImage generator width height
    let content = encodePng image
    res <- call twInfo mgr $ updateWithMedia (T.pack status) (MediaRequestBody "color.png" $ RequestBodyLBS content)
    print res
    delayMinutes 60
