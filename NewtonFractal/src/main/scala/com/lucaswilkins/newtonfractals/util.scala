package com.lucaswilkins.newtonfractals

import com.lucaswilkins.newtonfractals.complex._

/**
 * Created by lucas on 15/11/2014.
 */
object util {

  /**
   *  Zip two lists, when they are different lengths pad the shortest
   *  to a default value
   *
   * @param list1     First list
   * @param list2     Second list
   * @param default1  Value to pad first list with
   * @param default2  Value to pad second list with
   * @tparam S        Type of first list (and default)
   * @tparam T        Type of second list (and default)
   * @return          The two lists padded and zipped
   *
   */
  def zipPadded[S,T](list1: List[S], list2: List[T], default1: S, default2: T): List[Tuple2[S,T]] ={
    if(list1.length > list2.length) {
      list1 zip (list2 ::: List.fill(list1.length - list2.length)(default2))
    } else {
      (list1 ::: List.fill(list2.length - list1.length)(default1)) zip list2
    }
  }

  /**
   * Formatting of algabraic coefficients
   *
   * @param x   A complex coefficient
   * @param rhs The string represent the symbol which it multiplies
   * @return    A properly formatted coefficient string with symbol
   */

  def coefficientTypeAdditionFormat(x: Complex, rhs: String): String = {
    if (x == new Complex(0)) {
      ""
    } else if (x == new Complex(1)) {
      if(rhs=="1") "1" else s"$rhs"
    } else if (x.isComplex) {
      if(rhs=="1") s"($x)" else s"($x)${rhs}"
    } else {
      if(rhs=="1") s"$x" else s"$x${rhs}"
    }
  }

  /**
   * Utility function for formatting powers
   *
   * @param x     A symbol as a string
   * @param power The power
   * @return      representation of x**power
   */
  def formatPower(x: String, power: Int): String = {
    if(power == 0) {
      "1"
    } else if(power == 1) {
      x
    } else {
      s"$x^$power"
    }
  }

  /**
   * Launch a website in the default browser
   *
   * @param urlString String containing the url for the website
   */
  def launchWebsite(urlString: String) = {
    com.lucaswilkins.thirdparty.BareBonesBrowserLaunch.openURL(urlString)
  }
}
