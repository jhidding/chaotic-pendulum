module Plotting where

import Prelude
import Effect.Uncurried (EffectFn3, runEffectFn3, EffectFn4, runEffectFn4)
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
foreign import restyleUnc :: EffectFn4 String String (Array (Array Number)) (Array Int) Unit

lineChart :: String -> Array PlotData -> Layout -> Effect Unit
lineChart = runEffectFn3 lineChartUnc

restyle :: String -> String -> Array (Array Number) -> Array Int -> Effect Unit
restyle = runEffectFn4 restyleUnc

