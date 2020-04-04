module Plotting where

import Prelude
import Effect.Uncurried (EffectFn3, runEffectFn3)
import Effect (Effect)
-- import Data.Function.Uncurried (Fn4)

type PlotData =
    { x :: Array Number
    , y :: Array Number
    , mode :: String }

type Layout =
    { title :: String }
-- foreign import data Layout :: Type

foreign import lineChartUnc :: EffectFn3 String (Array PlotData) Layout Unit

lineChart :: String -> Array PlotData -> Layout -> Effect Unit
lineChart = runEffectFn3 lineChartUnc

