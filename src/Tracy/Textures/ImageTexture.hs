module Tracy.Textures.ImageTexture
  ( imageTexture
  , loadTextureImages
  )
where

import Control.Applicative (pure, (<$>), (<*>))
import Control.Lens
import Control.Monad (forM)
import Data.Colour
import Data.List (nub)
import qualified Data.Map as Map

import Codec.Picture
  ( DynamicImage(ImageRGB8)
  , PixelRGB8(..)
  , imageWidth
  , imageHeight
  , readPng
  , pixelAt
  )

import Tracy.Types

imageTexture :: Maybe TextureMapping -> ImageData -> Texture
imageTexture mm img =
    Texture { _getColor = imageGetColor mm img
            }

imageGetColor :: Maybe TextureMapping -> ImageData -> Shade -> Color
imageGetColor Nothing img sh =
    case (sh^.mappingU, sh^.mappingV) of
        (Just u, Just v) ->
            let row = fromEnum (v * (toEnum $ imageHeight (img^.imageBuffer) - 1))
                col = fromEnum (u * (toEnum $ imageWidth  (img^.imageBuffer) - 1))
            in if row < 0 || col < 0 || row > (imageHeight (img^.imageBuffer)) - 1 || col > (imageWidth (img^.imageBuffer)) - 1
               then cRed
               else imageGetColorAt img row col
        _ -> cRed
imageGetColor (Just theMapping) img sh =
    let (row, col) = (theMapping^.getTexelCoordinates) (sh^.localHitPoint)
                     (imageWidth (img^.imageBuffer))
                     (imageHeight (img^.imageBuffer))
    in if row < 0 || col < 0 || row > (imageHeight (img^.imageBuffer)) - 1 || col > (imageWidth (img^.imageBuffer)) - 1
       then cRed
       else imageGetColorAt img row col

imageGetColorAt :: ImageData -> Int -> Int -> Color
imageGetColorAt img row col = pixelToColor $ pixelAt (img^.imageBuffer) col row

pixelToColor :: PixelRGB8 -> Colour
pixelToColor (PixelRGB8 r g b) =
    Colour (f r) (f g) (f b)
    where
        f v = (toEnum $ fromEnum v) / 255.0

loadTextureImages :: (HasTextureImages a) => a -> IO ImageGroup
loadTextureImages v =
    Map.fromList <$>
    (forM (nub $ findTextureImages v) $ \p ->
        (,) <$> (pure p) <*> loadTextureImage p)

loadTextureImage :: FilePath -> IO ImageData
loadTextureImage filename = do
    result <- readPng filename
    case result of
        Left s -> fail s
        Right (ImageRGB8 img) -> return $ ImageData img
        Right _ -> fail "Wrong image type"
