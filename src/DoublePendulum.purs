-- ------ language="PureScript" file="src/DoublePendulum.purs" project://lit/pendulum.md#597
module DoublePendulum where

import Prelude
import Effect (Effect)
import Math (pow, cos, sin, pi, (%))

import Hamilton ( HamiltonianSystem, State, leapFrog, haltAtTime, iterateSolution )

import Flare as Flare
import Flare.Drawing (runFlareDrawing)
import Signal as Signal
import Signal.DOM (animationFrame)

import Graphics.Drawing as Drawing
import Color (rgb')

-- ------ begin <<double-pendulum>>[0] project://lit/pendulum.md#358
type DoublePendulum =
        { l1 :: Number
        , m1 :: Number
        , l2 :: Number
        , m2 :: Number
        , g  :: Number }
-- ------ end
-- ------ begin <<double-pendulum>>[1] project://lit/pendulum.md#369
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
-- ------ begin <<double-pendulum>>[2] project://lit/pendulum.md#425
kineticEnergy :: DoublePendulum -> Coordinates Number -> Coordinates Number -> Number
kineticEnergy z (Coordinates q) (Coordinates p) =
    (b * (pow p.theta 2.0) + a * (pow p.phi 2.0) - 2.0 * c * p.theta * p.phi) / (2.0 * det)
    where a = (z.m1 + z.m2) * (pow z.l1 2.0)
          b = z.m2 * (pow z.l2 2.0)
          c = z.m2 * z.l1 * z.l2 * (cos $ q.theta - q.phi)
          det = a * b - c * c
-- ------ end
-- ------ begin <<double-pendulum>>[3] project://lit/pendulum.md#455
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
-- ------ begin <<double-pendulum-animation>>[0] project://lit/pendulum.md#488
type Model =
    { state       :: State Coordinates
    , params      :: DoublePendulum
    , currentTime :: Number
    , timeStep    :: Number
    , playing     :: Boolean }
-- ------ end
-- ------ begin <<double-pendulum-animation>>[1] project://lit/pendulum.md#499
data Msg =
      AnimationFrame Number
    | TogglePlay
    | None
-- ------ end
-- ------ begin <<double-pendulum-animation>>[2] project://lit/pendulum.md#508
forwardModel :: Number -> Model -> Model
forwardModel dt m = m { state = moduloState newState
                      , currentTime = newTime }
    where newState = iterateSolution
            (leapFrog m.timeStep (doublePendulum m.params))
            (haltAtTime $ m.state.time + dt)
            m.state
          newTime = m.currentTime + newState.time - m.state.time

moduloState :: State Coordinates -> State Coordinates
moduloState s@{ position: Coordinates { theta, phi } } =
    s { position = Coordinates { theta: theta % (2.0*pi)
                               , phi: phi % (2.0*pi) } }

update :: Msg -> Model -> Model
update (AnimationFrame t) m = if (t / 1000.0 - m.currentTime) > m.timeStep
    then if m.playing
         then forwardModel (t / 1000.0 - m.currentTime) m
         else m { currentTime = t / 1000.0 }
    else m
update TogglePlay m = m { playing = not m.playing }
update None m = m
-- ------ end
-- ------ begin <<double-pendulum-animation>>[3] project://lit/pendulum.md#535
initModel :: Model
initModel =
    { state:  { position: Coordinates { theta: pi / 2.0, phi: 0.0 }
              , momentum: Coordinates { theta: 0.0,      phi: 0.0 }
              , time: 0.0 }
    , params: { l1: 1.6, l2: 1.2, m1: 2.0, m2: 1.5, g: 9.81 }
    , currentTime: 0.0
    , timeStep: 0.01
    , playing: false }
-- ------ end
-- ------ begin <<double-pendulum-flare>>[0] project://lit/pendulum.md#551
events :: Signal.Signal Number -> Flare.UI Msg
events time = Flare.liftSF (Signal.merge $ AnimationFrame <$> time)
                           (Flare.button "Play/Pause" None TogglePlay)
-- ------ end
-- ------ begin <<double-pendulum-flare>>[1] project://lit/pendulum.md#559
main :: Effect Unit
main = do
    time <- animationFrame
    let model = Flare.foldp update initModel (events time)
    runFlareDrawing "double-pendulum-control" 
                    "double-pendulum-output"
                    (draw <$> model)
-- ------ end
-- ------ begin <<double-pendulum-flare>>[2] project://lit/pendulum.md#571
draw :: Model -> Drawing.Drawing
draw { state:  { position: Coordinates q }
     , params: { l1, l2, m1, m2 } } =
    let p1    = { x: l1 * (sin q.theta), y: l1 * (cos q.theta) }
        p2    = { x: p1.x + l2 * (sin q.phi), y: p1.y + l2 * (cos q.phi) }
        orig  = { x: 0.0, y: 0.0 }
        tr {x, y} = { x: x*100.0 + 300.0, y: y*100.0 + 100.0 }
        rods  = Drawing.outlined 
                    (Drawing.lineWidth 2.0 <> Drawing.outlineColor (rgb' 0.0 0.0 0.0))
                    (Drawing.path $ map tr [orig, p1, p2])
        hinge = Drawing.filled
                    (Drawing.fillColor (rgb' 0.0 0.0 0.0))
                    (Drawing.circle (tr orig).x (tr orig).y 3.0)
        mass1 = Drawing.filled
                    (Drawing.fillColor (rgb' 0.5 0.0 0.0))
                    (Drawing.circle (tr p1).x (tr p1).y (m1 * 4.0))
        mass2 = Drawing.filled
                    (Drawing.fillColor (rgb' 0.5 0.0 0.0))
                    (Drawing.circle (tr p2).x (tr p2).y (m2 * 4.0))
    in Drawing.shadow (Drawing.shadowOffset 2.0 2.0 <> Drawing.shadowBlur 4.0 <> Drawing.shadowColor (rgb' 0.5 0.5 0.5))
                      (rods <> hinge <> mass1 <> mass2)
-- ------ end
-- ------ end
