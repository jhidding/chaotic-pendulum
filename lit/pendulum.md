---
title: Chaotic pendulum
author: Johan Hidding
---

$\renewcommand{\vec}[1]{{\bf #1}}$

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

## Leap-frog method

We can solve this using Leap-Frog method. We can describe this methods in terms of types in code. We deal with `Scalar`s and `Vector`s, for some an arbitrary distinction: a `Scalar` is a `Vector` for which we can guarantee the dimension is always 1.

``` {.pure #leap-frog}
newtype Scalar a = Scalar a

instance scalarFunctor :: Functor Scalar where
    map f (Scalar a) = Scalar (f a)

instance scalarApply :: Apply Scalar where
    apply (Scalar f) (Scalar a) = Scalar (f a)

instance scalarApplicative :: Applicative Scalar where
    pure a = Scalar a
```

We can describe the `State` of a system by giving `time`, `position` and `momentum`.

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

``` {.pure #leap-frog}
type Solver a = HamiltonianSystem a -> State a -> State a
```

The leap-frog method is a solver that uses a *kick* and *drift* phase. The *kick* phase integrates only the momentum part of the system,

``` {.pure #leap-frog}
kick :: forall f. (Applicative f) 
    => Number -> HamiltonianSystem f -> State f -> State f
kick dt system state = state
    { momentum = (\p dp -> p + dt * dp)
               <$> state.momentum
               <*> system.momentumEquation state }
```

while the *drift* phase only integrates the position part.

``` {.pure #leap-frog}
drift :: forall f. (Applicative f)
    => Number -> HamiltonianSystem f -> State f -> State f
drift dt system state = state
    { position = (\q dq -> q + dt * dq)
               <$> state.position
               <*> system.positionEquation state }
```

We add a *wait* phase to control the clock, in case we encounter time-dependent systems, and because we move the clock by `dt/2` after each *kick* or *drift*, which is specific to the leap-frog method.

``` {.pure #leap-frog}
wait :: forall a. Number -> State a -> State a
wait dt state = state
    { time = state.time + dt }
```

The leap-frog method follows a kick by a drift, with a separation of $dt/2$.

``` {.pure #leap-frog}
leapFrog :: forall f. (Applicative f)
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


``` {.pure #pendulum-model}
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
```

<div id="pendulum"></div>

<button class="btn btn-primary" type="button" data-toggle="collapse" data-target="#pendulum-main" aria-controls="pendulum-main">
&lt;&lt;pendulum-main&gt;&gt;=
</button>

::: {.collapse #pendulum-main}
:::: {.card style="height: 20em"}
::::: {.overflow-auto}
``` {.pure file=src/Pendulum.purs}
module Pendulum where

import Prelude
-- import Data.Array ((:))
import Effect (Effect)
import Math (pow, sin)

import Hamilton (HamiltonianSystem, Scalar (..), State, leapFrog, integrateSystem, haltAtTime)
import Plotting (lineChart, PlotData)

<<pendulum-model>>
```
:::::
::::
:::

<script src="https://cdn.plot.ly/plotly-latest.min.js"></script>
<script type="text/javascript" src="js/pendulum.js"></script>
<!-- <script>
var pendulum = document.getElementById("pendulum");
const app = Elm.Pendulum.init({ node: pendulum });
</script> -->

# The double pendulum

The double pendulum is more complicated than the single one. We now have two angles $\theta$ and $\varphi$, two lengths $k$ and $l$, and two masses $m$ and $n$. The potential energy is

$$U(\theta, \varphi) = - m g k \cos{\theta} - n g (k \cos{\theta} + l \cos{\varphi}),$$

and the kinetic energy (takes a bit of calculating),

$$T(\theta, \varphi, \dot{\theta}, \dot{\varphi}) = \frac{1}{2} m k^2 \dot{\theta}^2 + \frac{1}{2} n \left(\dot{\theta}^2 k^2 + \dot{\varphi}^2 l^2 + 2\dot{\theta}\dot{\varphi}kl \cos(\theta - \varphi)\right).$$

``` {.pure #double-pendulum}
type alias DoublePendulum =
        { l1 : Float
        , m1 : Float
        , l2 : Float
        , m2 : Float }
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
kineticEnergy : DoublePendulum -> State -> Float
kineticEnergy a s = 
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
doublePendulum : DoublePendulum -> HamiltonianSystem
doublePendulum a = let det q1 q2 = a.m2 * a.l1^2 * a.l2^2 * (a.m1 + a.m2 * (sin (q1 - q2))^2)
                       kinE q1 q2 p1 p2 = () / (2 * (det q1 q2))
                       xi q1 q2 p1 p2 = (a.m2 * a.l1 * a.l2 / (2 * (det q1 q2))) *
                                ((kinE q1 q2 p1 p2) * a.m2 * a.l1 * a.l2 * (sin (2 * (q1 - q2)))
                                - 2 * (sin (q1 - q2)) * p1 * p2)
    in { positionEquation = (\s -> 
```

