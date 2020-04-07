---
title: Chaotic pendulum
author: Johan Hidding
---

$\renewcommand{\vec}[1]{{\bf #1}}$

<div class="container-fluid">
::: {.row}
:::: {.col}
<div class="card h-100"><div class="card-body">
<h3 class="card-title">Literate Programming</h3>
<p class="card-text">Write prose and code intermixed. Not just some choice snippets: **all code is included!**
This document is a rendering of a completely **self-contained Markdown** file.</p>
</div>
<a href="https://entangled.github.io/" class="btn btn-primary mt-auto mx-4">About Entangled</a>
</div>
::::
:::: {.col}
<div class="card h-100"><div class="card-body">
<h3 class="card-title">Build with Pandoc</h3>
<p class="card-text">Pandoc is the **universal document converter**. This demo is converted from Markdown to a responsive Bootstrap website using Pandoc.</p>
</div>
<a href="https://pandoc.org/" class="btn btn-primary mt-auto mx-4">About Pandoc</a>
</div>
::::
:::: {.col}
<div class="card h-100"><div class="card-body">
<h3 class="card-title">Strongly typed Physics</h3>
<p class="card-text">This demonstrates some **strongly typed physics**, combining Hamiltonian dynamics with the elegant type system of PureScript.</p>
</div>
<a href="https://purescript.org/" class="btn btn-primary mt-auto mx-4">About PureScript</a>
</div>
::::
:::
</div>

-----

This demo shows an interactive model of the chaotic double pendulum, written in PureScript, using Pandoc and Entangled create the static web page. The entire demo runs in your web browser, so no server with Python or R is needed. Since PureScript is not the most well known programming language, I will explain some details about the use of PureScript in a bit more detail. You can recognize these by their blue shade.

::: {.alert .alert-info}
PureScript is a strongly typed functional language similar to Haskell that compiles to JavaScript. It has a highly formal approach to data types that matches really well with the topic of modelling dynamic systems.
:::

I understand the danger of explaining two difficult things at once. Non-linear dynamics is a difficult topic and PureScript is a language with high levels of abstraction. I'm hoping you can cherry pick from this demo whatever you're trying to learn.

## Why we care

- Experimental science blogging with live interactive demos
- How to use Literate Programming to learn and teach
- See a chaotic double pendulum in action

## Learn some Tech

- Learn some [PureScript](https://purescript.org)
- See how we can call [PlotLy](https://plotly.com) from PureScript
- Use the PureScript [Flare library](https://github.com/sharkdp/purescript-flare) to create demos
- Use Pandoc to create a static [Bootstrap](https://getbootstrap.com) page
- Use [Entangled](https://entangled.github.io) for literate programming

The physics used in this demo assumes some understanding of Lagrangian/Hamiltonian dynamics.

We will first model the time evolution of a simple pendulum, and then extend our model to that of a double pendulum.

# The pendulum

The pendulum still makes the world tick:

$$\mathcal{L}(t; \theta, \dot{\theta}) = \frac{1}{2}ml^2\dot{\theta}^2 + mgl \cos\theta.$$

Using the Euler-Lagrange equations, in very terse notation $D_t \partial_{\dot{\theta}} \mathcal{L} = \partial_{\theta} \mathcal{L}$, leads to

$$\ddot{\theta} = -\frac{g}{l} \sin\theta.$$

To solve this, we write down the equation in phase-space using position $\vec{q}(t) = \theta(t)$ and momentum $\vec{p}(t) = D_{\dot{\theta}} \mathcal{L} = ml^2\dot{\theta}(t)$.

And the Hamiltonian becomes

$$\mathcal{H} = \frac{\vec{p}^2}{2ml^2} - mgl \cos \vec{q}.$$

Having $\dot{\vec{p}} = - \partial_q \mathcal{H}$ and $\dot{\vec{q}} = + \partial_p \mathcal{H}$.

$$\begin{pmatrix}\dot{\vec{q}} \\ \dot{\vec{p}}\end{pmatrix} = \begin{pmatrix}\vec{p} / {ml^2} \\ -mgl \sin \vec{q}\end{pmatrix}.$$

In PureScript this system can be expressed as follows:

``` {.pure #pendulum-model}
simplePendulum :: Number -> Number -> HamiltonianSystem Scalar
simplePendulum g l =
    { positionEquation: (\s -> (\p -> p / pow l 2.0) <$> s.momentum)
    , momentumEquation: (\s -> (\q -> -g * l * (sin q)) <$> s.position)
    }
```

::: {.alert .alert-info}
The first line of this definition is the type declaration of the `simplePendulum` function: it takes a number (the gravitational accelleration) another number (the length of the pendulum) and returns a `HamiltonianSystem Scalar`, which is saying a Hamiltonian system of equations in one dimension. These types will be defined when we program the leap-frog integrator.
:::

We are now going to compute the time evolution of the single pendulum using the leap-frog method.

# Leap-frog method

We can describe this methods in terms of types in code. Later on, we will deal with systems of more than one dimension. To deal with this I assume the position and momentum vectors are stored in some `Apply` data structure. By doing this, I can write down the leap-frog integrator in generic terms. For the one-dimensional single pendulum, I have to wrap the `Number` into an `Apply` which I call `Scalar`.

``` {.pure #leap-frog}
newtype Scalar a = Scalar a

instance scalarFunctor :: Functor Scalar where
    map f (Scalar a) = Scalar (f a)

instance scalarApply :: Apply Scalar where
    apply (Scalar f) (Scalar a) = Scalar (f a)

instance scalarApplicative :: Applicative Scalar where
    pure a = Scalar a
```

::: {.alert .alert-info}
A `Functor` is a type that can be mapped over. This mapping should hold to the `Functor` laws. It should preserve the identity function:

``` {.pure}
map id = id
```

and be linear over function composition

``` {.pure}
map (f <<< g) = map f <<< map g
```

An `Applicative` is a type allows for combining `Functors` over functions with `Functors` over arguments. Combined with curried function application this lets us `map` over functions of multiple arguments. This is explained better than I ever could in the ["Learn you a Haskell" book chapter on Functors, Applicatives and Monoids](http://learnyouahaskell.com/functors-applicative-functors-and-monoids). Haskell and PureScript are similar enough for this reference to apply.
:::

We can describe the `State` of a system by giving `time`, `position` and `momentum`. Here `a` is the type-parameter that indicates the container type. For instance, the `position` member of a `State Scalar` will have type `Scalar Number`.

``` {.pure #leap-frog}
type State a =
    { time :: Number
    , position :: a Number
    , momentum :: a Number }
```

Then, a Hamiltonian system is given by two sets of first-order differential equations.

``` {.pure #leap-frog}
type HamiltonianSystem a =
    { positionEquation :: State a -> a Number
    , momentumEquation :: State a -> a Number }
```

And in general, a `Solver` for the Hamiltonian system takes a state object and returns a new state.

``` {.pure #leap-frog}
type Solver a = HamiltonianSystem a -> State a -> State a
```

The leap-frog method is a solver that uses a *kick* and *drift* phase. The *kick* phase integrates only the momentum part of the system,

``` {.pure #leap-frog}
kick :: forall f. (Apply f) 
    => Number -> HamiltonianSystem f -> State f -> State f
kick dt system state = state
    { momentum = (\p dp -> p + dt * dp)
               <$> state.momentum
               <*> system.momentumEquation state }
```

while the *drift* phase only integrates the position part.

``` {.pure #leap-frog}
drift :: forall f. (Apply f)
    => Number -> HamiltonianSystem f -> State f -> State f
drift dt system state = state
    { position = (\q dq -> q + dt * dq)
               <$> state.position
               <*> system.positionEquation state }
```

::: {.alert .alert-info}
Note that, for instance in the `drift` function, the operators `<$>` and `<*>` are our way of applying the lambda-function `(\q dq -> q + dt * dq)` to the `position` member of the state and the change in position given by the governing system of equations of motion. The `<$>` operator takes a function (of two arguments) and an applicative, call it a vector, and creates a vector of functions of one argument. Then the `<*>` operator applies the vector of functions to another vector of numbers. This way we can apply a function of $n$ arguments, point-wise to $n$ vectors, as long as these vectors are an instance of `Apply`.
:::

We add a *wait* phase to control the clock, in case we encounter time-dependent systems, and because we move the clock by `dt/2` after each *kick* or *drift*, which is specific to the leap-frog method.

``` {.pure #leap-frog}
wait :: forall a. Number -> State a -> State a
wait dt state = state
    { time = state.time + dt }
```

The leap-frog method follows a kick by a drift, with a separation of $dt/2$. We choose to start with a kick, then wait, drift, and wait again.

``` {.pure #leap-frog}
leapFrog :: forall f. (Apply f)
    => Number -> HamiltonianSystem f -> State f -> State f
leapFrog dt s = kick dt s >>> wait (dt/2.0) >>> drift dt s >>> wait (dt/2.0)
```

### Halting condition

``` {.pure #leap-frog}
type HaltingCondition a = State a -> State a -> Boolean

haltAtTime :: forall a. Number -> HaltingCondition a
haltAtTime t s1 s2 = s2.time >= t 
```

### Integration

``` {.pure #leap-frog}
iterateSolution :: forall a.
    (State a -> State a) -> HaltingCondition a -> State a -> State a
iterateSolution method halt init =
    let next = method init
    in if halt init next
        then next
        else iterateSolution method halt next

integrateSystem :: forall a.
    Solver a -> HaltingCondition a -> HamiltonianSystem a -> State a -> Array (State a)
integrateSystem solve halt f ic =
    let next = solve f ic
    in if halt ic next
        then ic : [next] :: Array (State a)
        else ic : integrateSystem solve halt f next
```

``` {.pure file=src/Hamilton.purs}
module Hamilton where

import Prelude
import Data.Array ((:))

<<leap-frog>>
```

## Integrating the solid pendulum

We had the following equations of motion,

Integrating this gives the time evolution of pendulum. We can compare it with the small-angle approximation. Here is a plot of the behaviour, you can change the initial angle with the slider.

<figure>
<div id="pendulum-plot"></div>
<div id="pendulum-control"></div>
</figure>

The source code for this interactive plot can be expanded below.

<button class="btn btn-primary" type="button" data-toggle="collapse" data-target="#pendulum-main" aria-controls="pendulum-main">
&lt;&lt;pendulum-main&gt;&gt;=
</button>

::: {.collapse #pendulum-main}
:::: {.card style="height: 20em"}
::::: {.overflow-auto}
``` {.pure file=src/Pendulum.purs}
module Pendulum where

import Prelude

import Effect (Effect)
import Math (pow, sin, sqrt, cos)
import Data.Array ((..))
import Data.Int (toNumber, round)
import Hamilton (HamiltonianSystem, Scalar (..), State, leapFrog, integrateSystem, haltAtTime)
import Plotting as Plotting

import Flare (runFlareWith, numberSlider)

<<pendulum-model>>

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
```
:::::
::::
:::

<script src="https://cdn.plot.ly/plotly-latest.min.js"></script>
<script type="text/javascript" src="js/pendulum.js"></script>

# The double pendulum

The double pendulum is more complicated than the single one. We now have two angles $\theta$ and $\varphi$, two lengths $k$ and $l$, and two masses $m$ and $n$. The potential energy is

$$U(\theta, \varphi) = - m g k \cos{\theta} - n g (k \cos{\theta} + l \cos{\varphi}),$$

and the kinetic energy (takes a bit of calculating),

$$T(\theta, \varphi, \dot{\theta}, \dot{\varphi}) = \frac{1}{2} m k^2 \dot{\theta}^2 + \frac{1}{2} n \left(\dot{\theta}^2 k^2 + \dot{\varphi}^2 l^2 + 2\dot{\theta}\dot{\varphi}kl \cos(\theta - \varphi)\right).$$

``` {.pure #double-pendulum}
type DoublePendulum =
        { l1 :: Number
        , m1 :: Number
        , l2 :: Number
        , m2 :: Number
        , g  :: Number }
```

We describe the pendulum in terms of the coordinates $\theta$ and $\varphi$.

``` {.pure #double-pendulum}
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
```

We choose the position vector $\vec{q} = (\theta, \varphi)$, then the canonical momentum is $\vec{p} = \partial_{\dot{\vec{q}}} \mathcal{L}$. This gives the terrible momentum vector,

$$\vec{p} = \begin{pmatrix}
mk^2\dot{\theta} + nk^2\dot{\theta} + nkl \dot{\varphi} \cos(\theta - \varphi)\\
nl^2\dot{\varphi} + nkl \dot{\theta} \cos(\theta - \varphi)
\end{pmatrix}.$$

We need to write the Hamiltonian $\mathcal{H}$ in terms of $\vec{q}$ and $\vec{p}$. This is possible. I stole and rewrote this derivation from [Diego Assencio's blog](https://diego.assencio.com/?index=e5ac36fcb129ce95a61f8e8ce0572dbf).

We can rewrite our expression for $\vec{p}$ in a matrix-vector form,

$$\vec{p} = B \dot{\vec{q}},$$

where

$$B = \begin{pmatrix}
(m + n)k^2 && nkl \cos(\theta - \varphi)\\ nkl \cos(\theta - \varphi) && nl^2
\end{pmatrix}.$$

Then we can write the velocities $\dot{\vec{q}} = B^{-1} \vec{p}$. The inverse of a $2\times2$ symmetric matrix is given by

$$\begin{pmatrix}
a && c \\ c && b\end{pmatrix}^{-1} = \frac{1}{ab - c^2} \begin{pmatrix} b && -c \\ -c && a
\end{pmatrix}.$$

We will write $$D = \det B = nk^2l^2 \left(m + n \sin^2(\theta - \varphi)\right).$$ From this expression for the determinant it is clear that $D \ge nmk^2l^2$, and that the inverse $B^{-1}$ exists.

Now it is helpful to write $T(\theta, \varphi, \dot{\theta}, \dot{\varphi})$ in terms of $a, b, c$ and $d$,

$$T = \frac{1}{2}\left(a\dot{\theta}^2 + b\dot{\varphi} + 2c\dot{\theta}\dot{\varphi}\right).$$

Inserting $\dot{\theta} = (p_{\theta}b - p_{\varphi}c)/D$ and $\dot{\varphi} = (p_{\varphi}a - p_{\varphi}c)/D$, things magically work out (making me feel I did too much work) and we get,

$$T = \frac{bp_{\theta}^2 + ap_{\varphi}^2 - 2cp_{\theta}p_{\varphi}}{2D},$$

``` {.pure #double-pendulum}
kineticEnergy :: DoublePendulum -> Coordinates Number -> Coordinates Number -> Number
kineticEnergy z (Coordinates q) (Coordinates p) =
    (b * (pow p.theta 2.0) + a * (pow p.phi 2.0) - 2.0 * c * p.theta * p.phi) / (2.0 * det)
    where a = (z.m1 + z.m2) * (pow z.l1 2.0)
          b = z.m2 * (pow z.l2 2.0)
          c = z.m2 * z.l1 * z.l2 * (cos $ q.theta - q.phi)
          det = a * b - c * c
```

and the Hamiltonian becomes

$$\mathcal{H} = \frac
{nl^2 p_{\theta}^2 + (m + n)k^2 p_{\varphi}^2 - 2nkl p_{\theta} p_{\varphi} \cos(\theta - \varphi)}
{2nk^2l^2 \left(m + n \sin^2(\theta - \varphi)\right)}
- (m + n)gk \cos{\theta} - ngl \cos{\varphi}.$$

Now we get the Hamilton equations of motion, $\dot{\vec{p}} = - \partial_q \mathcal{H}$ and $\dot{\vec{q}} = + \partial_p \mathcal{H}$,

$$\begin{align}
\dot{\theta} = \partial_{p_{\theta}} \mathcal{H} =& \frac{nl}{D} \left(l p_{\theta} - k\cos(\theta - \varphi) p_{\varphi}\right),\\
\dot{\varphi} = \partial_{p_{\varphi}} \mathcal{H} =& \frac{k}{D} \left((m+n)k p_{\varphi} - nl\cos(\theta - \varphi) p_{\theta}\right),\\
\dot{p}_{\theta} = -\partial_{\theta} \mathcal{H} =&\xi - (m+n)gk \sin \theta,\\
\dot{p}_{\varphi} = -\partial_{\varphi} \mathcal{H} =& -\xi - ngl \sin \varphi,
\end{align}$$

where 

$$\xi = \frac{nkl}{2D}\left(T\ nkl\sin(2(\theta - \varphi)) - 2\sin(\theta - \varphi) p_{\theta} p_{\varphi}\right).$$

``` {.pure #double-pendulum}
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
```

Terrible indeed. It is amazing how quickly the addition of another pendulum to the system just explodes in your face.

<!-- <canvas id="double-pendulum-output" width="300" height="300"></canvas> -->
<div id="double-pendulum-output"></div>
<div id="double-pendulum-control"></div>

``` {.pure file=src/DoublePendulum.purs}
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

<<double-pendulum>>

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
```

<script type="text/javascript" src="js/double-pendulum.js"></script>

