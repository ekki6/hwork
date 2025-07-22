-- learn physics w functional programming


type R = Double
type Time = R
type Position = R
type Velocity = R
type Acceleration = R

type Derivative = (R -> R) -> R -> R

derivative :: R -> Derivative
derivative dt f t = ( f (t + dt/2) - f (t - dt/2)) / dt

ff :: R -> R
ff x = x **2 / 2

gg :: R -> R
gg x = x ** 3


pos1 :: Time -> Position
pos1 t = if t < 0
         then 0
         else 5 * t**2

vel1Analytic :: Time -> Velocity
vel1Analytic t = 10 *t

acc1Analytic :: Time -> Acceleration
acc1Analytic t = 10

vel1Numerical :: Time -> Velocity
vel1Numerical t = derivative 0.1 pos1 t

acc1Numerical :: Time -> Acceleration
acc1Numerical t = derivative 0.1 vel1Numerical t


