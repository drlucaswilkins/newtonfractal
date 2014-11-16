package com.lucaswilkins.newtonfractals

import com.lucaswilkins.newtonfractals.complex._
import com.lucaswilkins.newtonfractals.util._

object polynomial {

  /**
   * Representation of a complex polynomial
   *
   * @param _coeffs Coefficients of the polynomial in order from x**order (head) to constant (tail)
   */
  case class Polynomial(_coeffs: List[Complex]) {
    /* Remove preceeding zeros from polynomial */
    private def removeLeadingZeros(list: List[Complex]): List[Complex] =
      list match {
        case Nil ⇒ Nil
        case head :: tail ⇒ if(head == 0) removeLeadingZeros(tail) else head :: tail
      }

    val coeffs = removeLeadingZeros(_coeffs)

    /* Coefficients in order from the constant term to the highest order */
    lazy val reversedCoeffs = coeffs.reverse

    /* Tail recursive evaluation of the polynomial, uses accumulator pattern */
    private def calculate(x: Complex, xpower: Complex, coeffs: List[Complex], accum: Complex): Complex =
      coeffs match {
        case Nil ⇒ accum
        case head :: tail ⇒ calculate(x, x*xpower, tail, (xpower*head) + accum)
      }

    /**
     * Evaluate at this position
     *
     * @param x Complex value at which to evaluate
     * @return  Value of P(x)
     */
    def apply(x: Complex) = calculate(x, 1, reversedCoeffs, 0)

    /**
     * @return Order of this polynomial
     */
    def order = coeffs.length - 1

    /*
     *   Polynomial Algebra
     */

    def +(other: Polynomial): Polynomial = Polynomial(
      zipPadded[Complex, Complex](reversedCoeffs, other.reversedCoeffs, 0, 0)
        .map(x ⇒ x._1 + x._2)
        .reverse)

    def constantMult(x: Complex): Polynomial = Polynomial(coeffs.map(x*_))
    def mulByZtoN(n: Int): Polynomial = {
      val zeros: List[Complex] = List.fill[Complex](n)(0)
      Polynomial(coeffs ::: zeros)
    }

    def *(other: Polynomial): Polynomial =
      other.reversedCoeffs
        .zipWithIndex                                             // associate each entry in the polynomial with a power
        .map { case (coeff, index) ⇒ (coeff, mulByZtoN(index)) } // This polynomial raised to appropriate power
        .map { case (coeff, poly) ⇒ poly.constantMult(coeff) }   // and scaled by the coefficient of the other poly
        .foldLeft(Polynomial(Nil))(_+_)                          // sum them up


    def unary_-(): Polynomial = Polynomial(coeffs.map(-_))

    def -(other: Polynomial): Polynomial = this.+(-other)

    override def equals(other: Any): Boolean = other match {
      case poly: Polynomial ⇒
        (coeffs zip poly.coeffs)
          .map { case (a,b) ⇒ a == b }
          .foldLeft(true)(_&&_)

      case _ ⇒ false
    }

    /**
     * Differentiate this polynomial with respect to its argument
     * @return Differentiated polynomial
     */
    def diff: Polynomial = Polynomial(
      reversedCoeffs
        .zipWithIndex
        .map(x ⇒ x._1 * x._2)
        .drop(1)
        .reverse)


    /* String representation... always a mess */
    override def toString = s"P(z) = " + (
        if(coeffs.length == 0) {
          "0"
        } else if(coeffs.length == 1){
          coeffs(0).toString
        } else {
          val nonZero = reversedCoeffs
            .zipWithIndex
            .filter(_._1 != 0)

          if (nonZero.length == 0) {
            "0"
          } else {

          val baseString = nonZero
            .reverse
            .map {
            case (coefficient, power) ⇒ (
              if (coefficient.shouldNegify) " - " else " + ",
              if (coefficient.shouldNegify) -coefficient else coefficient,
              power)}
            .map {
            case (sign, coefficient, power) ⇒ sign +
              coefficientTypeAdditionFormat(coefficient,
                formatPower("z", power))}
            .foldLeft("")(_+_)

          if(baseString.charAt(1) == '-')
            "-"+baseString.substring(3)
          else
            baseString.substring(3)
        }
      })
  }

  object Polynomial {
    /**
     * @param roots Roots of the polynomial
     * @return      A complex polynomial
     */
    def fromRoots(roots: List[Complex]): Polynomial = {
      val asTerms = roots.map(num2complex(1) :: -_ :: Nil).map(Polynomial(_))
      asTerms match {
        case Nil ⇒ Polynomial(0 :: Nil)
        case nonZeroList ⇒ nonZeroList.reduceLeft(_*_)
      }
    }
  }

  implicit def num2poly[T](x: T)(implicit h: Numeric[T]) =
    Polynomial(h.toDouble(x) :: Nil)

  implicit def complex2poly(x: Complex) = Polynomial(x::Nil)

  def polyVariable = new Polynomial(List[Complex](1,0))

}