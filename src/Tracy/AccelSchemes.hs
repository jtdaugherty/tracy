module Tracy.AccelSchemes
  ( module Tracy.Grid
  , noScheme
  , accelSchemes
  )
  where

import Tracy.Types
import Tracy.Grid

noScheme :: AccelScheme
noScheme = AccelScheme "none" id

accelSchemes :: [AccelScheme]
accelSchemes =
    [ noScheme
    , gridScheme
    ]

