package com.lucaswilkins.newtonfractals

import java.text.DecimalFormat

/**
 * Complex numbers, should be fairly self explanatory
 */
object complex {
  case class Complex(val re: Double, val im: Double = 0) {
    def conj = Complex(re, -im)
    def unary_- = Complex(-re, -im)
    def magSqr = re*re + im*im
    def mag = scala.math.sqrt(magSqr)
    def +(other: Complex) = Complex(re+other.re, im+other.im)
    def -(other: Complex) = Complex(re-other.re, im-other.im)
    def *(other: Complex) = Complex(re*other.re - im*other.im, re*other.im + im*other.re)
    def /(other: Complex) = *(other.conj)*(1/other.magSqr)


    lazy val formatter = new DecimalFormat("#.###")
    override def toString: String =
      if((re == 0) && (im == 0)) {
        "0"
      } else if (re == 0) {
        if(im == 1)
          "i"
        else if(im == -1)
          "-i"
        else
          s"${formatter.format(im)}i"
      } else if(im == 0) {
        s"${formatter.format(re)}"
      } else {
        if(im == 1)
          s"${formatter.format(re)}+i"
        if(im == -1)
          s"${formatter.format(re)}-i"
        else if(im > 0)
          s"${formatter.format(re)}+${formatter.format(im)}i"
        else
          s"${formatter.format(re)}-${formatter.format(-im)}i"
      }

    def isReal = im == 0
    def isComplex = (im != 0) && (re != 0)
    def isImag = (re == 0) && (im != 0)

    override def equals(other: Any) = other match {
      case other: Complex ⇒ (other.re == re) && (other.im == im)
      case other: Double ⇒ (re == other) && (im == 0)
      case other: Float ⇒ (re == other) && (im == 0)
      case other: Long ⇒ (re == other) && (im == 0)
      case other: Int ⇒ (re == other) && (im == 0)
      case _ ⇒ false
    }

    /**
     * Whether it makes sense to use minus in equations
     * rather than plus.
     *
     * @return
     */
    def shouldNegify: Boolean = if(re == 0) {
      im < 0
    } else {
      re < 0
    }

  }

  implicit def num2complex[T](x: T)(implicit h: Numeric[T]): Complex =
    Complex(h.toDouble(x), 0)

  val i = Complex(0,1)

  implicit class PrependI[T](x:T)(implicit h: Numeric[T]) {
    def i = Complex(0, h.toDouble(x))
  }

}
