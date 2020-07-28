{-# LANGUAGE LambdaCase #-}

module Main where

import System.IO
import Data.List as DL
import Graphics.Image as GI
import Graphics.Image.IO as GIO
import Graphics.Image.Interface as GII

data Pair a b = Pair a b deriving (Eq, Show)
-- define a variant of pair where second value is omitted from comparison
-- this is done because (Double, Pixel) requires Pixel to have Ord
instance (Ord a, Eq b) => Ord (Pair a b) where
  compare (Pair a _) (Pair a' _) = compare a a'

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStr "Path to image: "
  path <- getLine
  image <- GI.readImageRGB GI.VU path
  putStr "Glitch vertically or horizontally(v/h)? "
  direction <- getLine
  let processedImg = (unpackBrightness . DL.sort) (avgBrightness image direction)
  writeImage (rename path ++ "Glitch.png") (packImg processedImg direction)
  putStrLn "Processing complete "

-- turn lists into image, transpose back if needed
packImg :: [[Pixel RGB Double]] -> String -> Image VU RGB Double
packImg list = \case
  "v" -> GI.fromLists list :: Image VU RGB Double
  "h" -> GI.fromLists (DL.transpose list) :: Image VU RGB Double

-- unfold pair after sorting
unpackBrightness :: [Pair Double [Pixel RGB Double]] -> [[Pixel RGB Double]]
unpackBrightness = DL.map (\(Pair a b) -> b)

-- turn image into lists; wrap inner list into pair of its brightness and itself
avgBrightness :: Image VU RGB Double -> String -> [Pair Double [Pixel RGB Double]]
avgBrightness img = \case
  "v" -> go img
  "h" -> go $ GI.transpose img
  where go = DL.map (\x -> Pair (DL.sum (DL.map brightness x) / fromIntegral (length x)) x) . GI.toLists

-- calculate brightness of a single pixel
brightness :: Pixel RGB Double -> Double
brightness p = 0.2126 * r + 0.7152 * g + 0.0722 * b
  where r = getPxC p RedRGB
        g = getPxC p GreenRGB
        b = getPxC p BlueRGB

rename :: String -> String -- take filename before '.'
rename [] = []
rename (c:cs) | c == '.' = []
              | otherwise = c : rename cs
