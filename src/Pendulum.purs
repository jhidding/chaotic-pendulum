-- ------ language="PureScript" file="src/Pendulum.purs" project://lit/pendulum.md#285
module Pendulum where

import Prelude

import Effect (Effect)
import Math (pow, sin, sqrt, cos)
import Data.Array ((..))
import Data.Int (toNumber, round)
import Hamilton (HamiltonianSystem, Scalar (..), State, leapFrog, integrateSystem, haltAtTime)
import Plotting as Plotting

import Flare (runFlareWith, numberSlider)

-- ------ begin <<pendulum-model>>[0] project://lit/pendulum.md#122
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
        small = smallAngle m
    Plotting.restyle "pendulum-plot" "y" [map (\{position: Scalar q} -> q) result] [0]
    Plotting.restyle "pendulum-plot" "y" [small.y] [1]

initModel :: Model
initModel = { initAngle: 3.0, dt: 0.1, tEnd: 10.0, length: 1.0 }

convertResult :: Array (State Scalar) -> Plotting.PlotData
convertResult rs =
    { x: map (\{time} -> time + initModel.dt/2.0) rs
    , y: map (\{position: Scalar q} -> q) rs
    , mode: "lines+markers" }

smallAngle :: Model -> Plotting.PlotData
smallAngle m =
    let period = sqrt $ m.length / 9.81
        ts = (\i -> m.dt * (toNumber i)) <$> 0..(round $ m.tEnd / m.dt)
        ys = (\t -> m.initAngle * (cos $ t / period)) <$> ts
    in { x: ts, y: ys, mode: "lines" }

main :: Effect Unit
main = do
    let result = integrateSystem (leapFrog initModel.dt) (haltAtTime 10.0)
                                 (simplePendulum 9.81 1.0)
                                 { time: 0.0, position: Scalar 3.0, momentum: Scalar 0.0 }
        slider = numberSlider "initial angle" 0.01 3.14 0.01 1.57

    Plotting.lineChart "pendulum-plot" [convertResult result, smallAngle initModel] { title: "pendulum" }
    runFlareWith "pendulum-control" updatePlot slider
-- ------ end
