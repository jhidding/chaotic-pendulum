-- ------ language="PureScript" file="src/DoublePendulum.purs" project://lit/pendulum.md#464
module DoublePendulum where

import Prelude
import Effect (Effect)
import Math (pow, cos, sin, pi)

import Hamilton ( HamiltonianSystem, State, integrateSystem, leapFrog, haltAtTime
                , iterateSolution )
-- import Plotting (lineChart)

import Flare as Flare
import Flare.Drawing (runFlareDrawing)
import Signal as Signal
import Signal.DOM (animationFrame)

import Graphics.Drawing as Drawing
import Color (rgb')

-- ------ begin <<double-pendulum>>[0] project://lit/pendulum.md#334
type DoublePendulum =
        { l1 :: Number
        , m1 :: Number
        , l2 :: Number
        , m2 :: Number
        , g  :: Number }
-- ------ end
-- ------ begin <<double-pendulum>>[1] project://lit/pendulum.md#345
newtype Coordinates a = Coordinates
        { theta :: a
        , phi   :: a }

instance functorCoordinates :: Functor Coordinates where
    map f (Coordinates { theta, phi }) =
        Coordinates { theta: f theta, phi: f phi }

instance applyCoordinates :: Apply Coordinates where
    apply (Coordinates { theta: fTheta, phi: fPhi })
          (Coordinates { theta, phi }) =
        Coordinates { theta: fTheta theta, phi: fPhi phi }

instance applicativeCoordinates :: Applicative Coordinates where
    pure a = Coordinates { theta: a, phi: a }

instance showCoordinates :: Show a => Show (Coordinates a) where
    show (Coordinates x) = show x
-- ------ end
-- ------ begin <<double-pendulum>>[2] project://lit/pendulum.md#401
kineticEnergy :: DoublePendulum -> Coordinates Number -> Coordinates Number -> Number
kineticEnergy z (Coordinates q) (Coordinates p) =
    (b * (pow p.theta 2.0) + a * (pow p.phi 2.0) - 2.0 * c * p.theta * p.phi) / (2.0 * det)
    where a = (z.m1 + z.m2) * (pow z.l1 2.0)
          b = z.m2 * (pow z.l2 2.0)
          c = z.m2 * z.l1 * z.l2 * (cos $ q.theta - q.phi)
          det = a * b - c * c
-- ------ end
-- ------ begin <<double-pendulum>>[3] project://lit/pendulum.md#431
doublePendulum :: DoublePendulum -> HamiltonianSystem Coordinates
doublePendulum z = { positionEquation, momentumEquation }
    where positionEquation { position: Coordinates q, momentum: Coordinates p } = 
              Coordinates { theta: dtQTheta, phi: dtQPhi }
              where dtQTheta = z.m2 * z.l2 / (det q) * (z.l2 * p.theta
                             - z.l1 * (mu q) * p.phi)
                    dtQPhi   = z.l1 / (det q) * ((z.m1 + z.m2) * z.l1 * p.phi
                             - z.m2 * z.l2 * (mu q) * p.theta)

          momentumEquation { position: Coordinates q, momentum: Coordinates p } = 
             Coordinates { theta: dtPTheta, phi: dtPPhi }
             where dtPTheta = xi - (z.m1 + z.m2) * z.g * z.m1 * (sin q.theta)
                   dtPPhi   = - xi - z.m2 * z.g * z.l2 * (sin q.phi)
                   xi       = z.m2 * z.l1 * z.l2 / (2.0 * (det q))
                            * ((kineticEnergy z (Coordinates q) (Coordinates p)) * z.m2 * z.l1 * z.l2 
                               * (sin $ 2.0 * (q.theta - q.phi))
                               - 2.0 * (sin $ q.theta - q.phi) * p.theta * p.phi)

          mu q  = cos $ q.theta - q.phi

          det q = a * b - c * c
              where a = (z.m1 + z.m2) * (pow z.l1 2.0)
                    b = z.m2 * (pow z.l2 2.0)
                    c = z.m2 * z.l1 * z.l2 * (mu q)
-- ------ end

type Model =
    { state       :: State Coordinates
    , params      :: DoublePendulum
    , currentTime :: Number
    , timeStep    :: Number
    , playing     :: Boolean }

data Msg =
      AnimationFrame Number
    | TogglePlay
    | None

initModel :: Model
initModel =
    { state:  { position: Coordinates { theta: pi / 2.0, phi: 0.0 }
              , momentum: Coordinates { theta: 0.0,      phi: 0.0 }
              , time: 0.0 }
    , params: { l1: 1.0, l2: 1.0, m1: 1.0, m2: 1.0, g: 9.81 }
    , currentTime: 0.0
    , timeStep: 0.02
    , playing: true }

forwardModel :: Number -> Model -> Model
forwardModel dt m = m { state = newState, currentTime = newTime }
    where newState = iterateSolution
            (leapFrog m.timeStep (doublePendulum m.params))
            (haltAtTime $ m.state.time + dt)
            m.state
          newTime = m.currentTime + newState.time - m.state.time

update :: Msg -> Model -> Model
update (AnimationFrame t) m = if m.playing
    then forwardModel (t / 1000.0 - m.currentTime) m
    else m { currentTime = t / 1000.0 }
update TogglePlay m = m { playing = not m.playing }
update None m = m

draw :: Model -> Drawing.Drawing
draw { state:  { position: Coordinates q }
     , params: { l1, l2 } } =
    let path = Drawing.path
            [ { x: 0.0,                y: 0.0 }
            , { x: l1*100.0 * (sin q.theta), y: l1 * 100.0 * (cos q.theta) }
            , { x: l1 *100.0* (sin q.theta) + l2 * 100.0 * (sin q.phi)
              , y: l1 *100.0* (cos q.theta) + l2 * 100.0 * (cos q.phi) } ]
        style = Drawing.lineWidth 2.0 <> Drawing.outlineColor (rgb' 0.0 0.0 0.0)
    in Drawing.outlined style path

events :: Signal.Signal Number -> Flare.UI Msg
events time = Flare.liftSF (Signal.merge $ AnimationFrame <$> time)
                           (Flare.button "Play" None TogglePlay)

main :: Effect Unit
main = do
    time <- animationFrame
    let model = Flare.foldp update initModel (events time)
    {- runFlareDrawing "double-pendulum-control" 
                    "double-pendulum-output"
                    (draw <$> model) -}
    Flare.runFlareShow "double-pendulum-control"
                       "double-pendulum-output"
                       model
-- ------ end
