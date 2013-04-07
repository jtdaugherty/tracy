module Tracy.Util where

import Control.Applicative
import Data.Colour
import qualified Data.ByteString as B

getColorBytes :: Colour -> B.ByteString
getColorBytes (Colour r g b) =
    B.pack $ (toEnum . fromEnum) <$> [ r * 255, g * 255, b * 255, 255 ]
