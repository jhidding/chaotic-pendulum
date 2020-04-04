-- ------ language="PureScript" file="src/Pendulum.purs" project://lit/pendulum.md#168
module Pendulum where

import Prelude
-- import Data.Array ((:))
import Effect (Effect)
import Math (pow, sin)

import Hamilton (HamiltonianSystem, Scalar (..), State, leapFrog, integrateSystem, haltAtTime)
import Plotting (lineChart, PlotData)

-- ------ begin <<pendulum-model>>[0] project://lit/pendulum.md#138
simplePendulum :: Number -> Number -> HamiltonianSystem Scalar
simplePendulum g l =
    { positionEquation: (\s -> (\p -> p / pow l 2.0) <$> s.momentum)
    , momentumEquation: (\s -> (\q -> -g * l * (sin q)) <$> s.position)
    }

convertResult :: Array (State Scalar) -> PlotData
convertResult rs =
    { x: map _.time rs
    , y: map (\{position: Scalar q} -> q) rs
    , mode: "lines+markers" }

main :: Effect Unit
main = do
    let result = integrateSystem (leapFrog 0.1) (haltAtTime 10.0)
                                 (simplePendulum 9.81 1.0)
                                 { time: 0.0, position: Scalar 3.0, momentum: Scalar 0.0 }
    lineChart "pendulum" [convertResult result] { title: "pendulum" }
-- ------ end
-- ------ end
