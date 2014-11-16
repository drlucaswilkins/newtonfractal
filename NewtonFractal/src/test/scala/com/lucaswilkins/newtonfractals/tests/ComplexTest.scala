package com.lucaswilkins.newtonfractals.tests

import com.lucaswilkins.newtonfractals.complex._

import org.scalatest._

/**
 * Some basic tests for the complex number class
 */
class ComplexTest extends FlatSpec with Matchers {
  val zero = new Complex(0)
  val one = new Complex(1)

  val c = new Complex(3,2)
  val b = new Complex(2,4)

  "Complex numbers" should "add linearly" in {
    one + one should equal (new Complex(2))
    one + new Complex(0,1) should equal (new Complex(1,1))
    zero + new Complex(1,1) should equal (new Complex(1,1))
  }

  it should "have have a real conjugate-self-product" in {
    (c.conj*c).isReal should be (true)
    (b.conj*b).isReal should be (true)
  }

  it should "have inverse to addition" in {
    (c-b) should equal (c+(-b))
    (b-1) should equal (b+(-1))
  }

  it should "obey some simple equalities involving i" in {
    (new Complex(3, 2)*i) should be (new Complex(-2, 3))
    (new Complex(7,-2)*i) should be (new Complex( 2, 7))
    (new Complex(5, 1)*i) should be (new Complex(-1, 5))
  }

  it should "implicitly convert appended is" in {
    (4 + (3 i)) should equal (new Complex(4,3))
    (4 - (7 i)) should equal (new Complex(4,-7))
    (2 + (3.14 i)) should equal (new Complex(2,3.14))
  }

  it should "print out correctly" in {
    (one.toString) should equal ("1")
    (i.toString) should equal ("i")
    (zero.toString) should equal ("0")
    ((2*i).toString) should equal ("2i")
    ((2*one).toString) should equal ("2")
    ((new Complex(2,3)).toString) should equal ("2+3i")

  }

}
