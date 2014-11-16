package com.lucaswilkins.newtonfractals

import com.lucaswilkins.newtonfractals.complex.Complex

/**
 * Created by lucas on 15/11/2014.
 */
class ControlNode(var x: Double, var y: Double) {

  def getScreenPosition(w: Int, h: Int) = ((w*x).toInt, (h*y).toInt)

  def setFromScreenPosition(w: Int, h: Int, i: Int, j: Int) = {
    x = i.toDouble / w
    y = j.toDouble / h
  }

  def asComplex = Complex(x,y)

}
