package com.lucaswilkins.newtonfractals

import com.lucaswilkins.newtonfractals.complex.Complex
import com.lucaswilkins.newtonfractals.polynomial._

/**
 * Newton-Raphson Solver for complex Polynomials
 */
class Newton(val poly: Polynomial, iters: Int = 10) {
  val deriv = poly.diff

  def solve(x: Complex, remainingIters: Int = iters): Tuple2[Complex, Double] = {
    if(remainingIters <= 0){
      (x, (poly(x)/deriv(x)).mag)
    } else {
      solve(x - (poly(x)/deriv(x)), remainingIters - 1)
    }
  }
}
