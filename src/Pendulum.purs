-- ------ language="PureScript" file="src/Pendulum.purs" project://lit/pendulum.md#162
module Pendulum where

import Prelude

import Effect (Effect)
import Math (pow, sin)

import Hamilton (HamiltonianSystem, Scalar (..), State, leapFrog, integrateSystem, haltAtTime)
import Plotting as Plotting

import Flare (runFlareWith, numberSlider)

-- ------ begin <<pendulum-model>>[0] project://lit/pendulum.md#144
simplePendulum :: Number -> Number -> HamiltonianSystem Scalar
simplePendulum g l =
    { positionEquation: (\s -> (\p -> p / pow l 2.0) <$> s.momentum)
    , momentumEquation: (\s -> (\q -> -g * l * (sin q)) <$> s.position)
    }
-- ------ end

type Model =
        { initAngle :: Number
        , dt :: Number
        , tEnd :: Number
        , length :: Number }

updatePlot :: Number -> Effect Unit
updatePlot iq = do
    let m = initModel { initAngle = iq }
        result = integrateSystem (leapFrog m.dt) (haltAtTime m.tEnd)
                                 (simplePendulum 9.81 m.length)
                                 { time: 0.0, position: Scalar m.initAngle, momentum: Scalar 0.0 }
    Plotting.restyle "pendulum-plot" "y" [map (\{position: Scalar q} -> q) result]

initModel :: Model
initModel = { initAngle: 3.0, dt: 0.1, tEnd: 10.0, length: 1.0 }

convertResult :: Array (State Scalar) -> Plotting.PlotData
convertResult rs =
    { x: map _.time rs
    , y: map (\{position: Scalar q} -> q) rs
    , mode: "lines+markers" }

main :: Effect Unit
main = do
    let result = integrateSystem (leapFrog 0.1) (haltAtTime 10.0)
                                 (simplePendulum 9.81 1.0)
                                 { time: 0.0, position: Scalar 3.0, momentum: Scalar 0.0 }
        slider = numberSlider "initial angle" 0.0 3.14 0.01 1.57

    Plotting.lineChart "pendulum-plot" [convertResult result] { title: "pendulum" }
    runFlareWith "pendulum-control" updatePlot slider
-- ------ end
