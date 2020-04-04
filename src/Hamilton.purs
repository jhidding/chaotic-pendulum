-- ------ language="PureScript" file="src/Hamilton.purs" project://lit/pendulum.md#132
module Hamilton where

import Prelude
import Data.Array ((:))

-- ------ begin <<leap-frog>>[0] project://lit/pendulum.md#35
newtype Scalar a = Scalar a

instance scalarFunctor :: Functor Scalar where
    map f (Scalar a) = Scalar (f a)

instance scalarApply :: Apply Scalar where
    apply (Scalar f) (Scalar a) = Scalar (f a)

instance scalarApplicative :: Applicative Scalar where
    pure a = Scalar a
-- ------ end
-- ------ begin <<leap-frog>>[1] project://lit/pendulum.md#50
type State a =
    { time :: Number
    , position :: a Number
    , momentum :: a Number }
-- ------ end
-- ------ begin <<leap-frog>>[2] project://lit/pendulum.md#59
type HamiltonianSystem a =
    { positionEquation :: State a -> a Number
    , momentumEquation :: State a -> a Number }
-- ------ end
-- ------ begin <<leap-frog>>[3] project://lit/pendulum.md#67
type Solver a = HamiltonianSystem a -> State a -> State a
-- ------ end
-- ------ begin <<leap-frog>>[4] project://lit/pendulum.md#73
kick :: forall f. (Applicative f) 
    => Number -> HamiltonianSystem f -> State f -> State f
kick dt system state = state
    { momentum = (\p dp -> p + dt * dp)
               <$> state.momentum
               <*> system.momentumEquation state }
-- ------ end
-- ------ begin <<leap-frog>>[5] project://lit/pendulum.md#84
drift :: forall f. (Applicative f)
    => Number -> HamiltonianSystem f -> State f -> State f
drift dt system state = state
    { position = (\q dq -> q + dt * dq)
               <$> state.position
               <*> system.positionEquation state }
-- ------ end
-- ------ begin <<leap-frog>>[6] project://lit/pendulum.md#97
wait :: forall a. Number -> State a -> State a
wait dt state = state
    { time = state.time + dt }
-- ------ end
-- ------ begin <<leap-frog>>[7] project://lit/pendulum.md#105
leapFrog :: forall f. (Applicative f)
    => Number -> HamiltonianSystem f -> State f -> State f
leapFrog dt s = kick dt s >>> wait (dt/2.0) >>> drift dt s >>> wait (dt/2.0)
-- ------ end
-- ------ begin <<leap-frog>>[8] project://lit/pendulum.md#113
type HaltingCondition a = State a -> State a -> Boolean

haltAtTime :: forall a. Number -> HaltingCondition a
haltAtTime t s1 s2 = s2.time >= t 
-- ------ end
-- ------ begin <<leap-frog>>[9] project://lit/pendulum.md#122
integrateSystem :: forall a.
    Solver a -> HaltingCondition a -> HamiltonianSystem a -> State a -> Array (State a)
integrateSystem solve halt f ic =
    let next = solve f ic
    in if halt ic next
        then ic : [next] :: Array (State a)
        else ic : integrateSystem solve halt f next
-- ------ end
-- ------ end
