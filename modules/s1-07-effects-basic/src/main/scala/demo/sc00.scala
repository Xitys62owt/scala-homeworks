package sc00

import scala.util.Try


// Types of Lazy Calculation coding
type  LazyCalcFunction[A] = () => A
class LazyCalcCapsule[A](val run: () => A)


// Functional Effect coding
case class FuncEffect[A](run: () => A)


// Safe Functional Effect
case class FuncEffectWithFallback[E, A](runSafe: () => Either[E, A]) {
    def runUnsafe = runSafe().right.get
}
case class FuncEffectErrorControled[A](runSafe: () => Try[A]) {
    def runUnsafe = runSafe().get
}

    
