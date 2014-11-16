package com.lucaswilkins.newtonfractals.tests

import com.lucaswilkins.newtonfractals.polynomial._
import com.lucaswilkins.newtonfractals.complex._
import org.scalatest._

/**
 * Very rough test for polynomials, we'll assume that complex numbers
 * work correctly for this test, and mostly use real polynomials represented
 * as complex.
 */
class PolynomialTest extends FlatSpec with Matchers {
  val b = new Polynomial(List[Complex](3,2,1))
  val c = new Polynomial(List[Complex](6,5,4,3))

  "Polynomials" should "add correctly" in {
    b+b should equal (new Polynomial(List[Complex](6,4,2)))
    b+c should equal (new Polynomial(List[Complex](6,8,6,4)))
  }

  it should "remove trailing values" in {
    ((Polynomial(List[Complex](1,0,0,0,0,1)) - Polynomial(List[Complex](1,0,0,0,0,0)))
      should equal (new Polynomial(1::Nil)))

    ((Polynomial(List[Complex](1,0,0,0,0,1)) - Polynomial(List[Complex](1,0,0,0,0,0)))
      .coeffs.length should equal (1))
  }

  it should "multiply correctly" in {
    (Polynomial(List[Complex](1,1))*Polynomial(List[Complex](1,-1))
      should equal (Polynomial(List[Complex](1,0,-1))))

    (Polynomial(List[Complex](1,i))*Polynomial(List[Complex](1,-i))
      should equal (Polynomial(List[Complex](1,0,1))))

  }

  it should "have a correctly defined 'polyVariable' entry" in {
    val z = polyVariable

    (z-1)*(z+1) should equal (z*z - 1)
  }

  it should "give the correct order" in {
    val z = polyVariable

    var variable: Polynomial = 1

    for(ind ← 0 until 10) {
      variable.order should be (ind)
      variable *= z
    }
  }

  it should "differentiate correctly" in {
    val z = polyVariable

    (z*z*z).diff should equal (3*z*z)
    ((z+1)*(z+1)).diff should equal (2*(z+1))

  }

  it should "print correctly" in {
    val z = polyVariable

    val startPosition = Polynomial(0::Nil).toString.length - 1

    def f(s:Any) = s.toString.substring(startPosition)

    f(z-1) should equal ("z - 1")
    f(1-z) should equal ("-z + 1")
    f(i*z*z - z + Complex(1,2)) should equal ("iz^2 - z + (1+2i)")
    f(i*z*z - z + Complex(0,-2)) should equal ("iz^2 - z - 2i")
    f(-i*z*z - z + Complex(0,-2)) should equal ("-iz^2 - z - 2i")
    f((i-2)*z*z - z + Complex(0,-2)) should equal ("-(2-i)z^2 - z - 2i")
    f((i-2)*z*z - (3+2*i)*z + (-4+5*i)) should equal ("-(2-i)z^2 - (3+2i)z - (4-5i)")
  }


}
