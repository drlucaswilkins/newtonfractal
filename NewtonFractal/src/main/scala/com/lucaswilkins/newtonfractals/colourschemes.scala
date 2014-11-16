package com.lucaswilkins.newtonfractals

import java.awt.Color

import scala.math._

object colourschemes {

  class Hues(val xSize: Int, val ySize: Int) extends ColourScheme {

    def positionColour(a: Double, b: Double, intensity: Double = 1): Color = {
      val p = a - 0.5
      val q = b - 0.5
      val angle = 0.5 * atan2(p, q) / Pi
      val radiusRaw = 1.4 * sqrt(p * p + q * q)
      val radius = if (radiusRaw > 1) 1 else radiusRaw
      Color.getHSBColor(angle.toFloat, 1, scala.math.pow(radius, 0.3).toFloat)
    }

  }

  class Hues2(val xSize: Int, val ySize: Int) extends ColourScheme {

    def positionColour(a: Double, b: Double, intensity: Double = 1): Color = {
      val angle = 2 * atan2(a, b) / Pi
      val radiusRaw = 0.8*sqrt(a * a + b * b)
      val radius = if (radiusRaw > 1) 1 else radiusRaw
      Color.getHSBColor(angle.toFloat, 1, scala.math.pow(radius, 0.3).toFloat)
    }

  }

  class BlueGreens(val xSize: Int, val ySize: Int) extends ColourScheme {

    def positionColour(a: Double, b: Double, intensity: Double = 1): Color = {
      val p = sin(4*Pi*a)
      val q = sin(4*Pi*b)
      new Color(0, (p*p).toFloat, (q*q).toFloat)
    }

  }

  class Fire(val xSize: Int, val ySize: Int) extends ColourScheme {

    def positionColour(a: Double, b: Double, intensity: Double = 1): Color = {
      val p = sin(10*Pi*a)
      val q = sin(10*Pi*b)
      new Color(0, (p*p).toFloat, (0.5+0.5*q*q).toFloat)
    }

    override def bySolution(a: Double, b: Double, intensity: Double): Int = {
      val colour: Color = new Color(super.bySolution(a,b,intensity))
      new Color(255-colour.getRed, 255-colour.getGreen, 255-colour.getBlue).getRGB
    }
  }

  class RawRGB(val xSize: Int, val ySize: Int) extends ColourScheme {

    def positionColour(a: Double, b: Double, intensity: Double = 0): Color = {
      new Color(a.toFloat, b.toFloat, intensity.toFloat)
    }

    override def bySolution(a: Double, b: Double, intensity: Double): Int = {
      val aa = if(a < 0) 0 else if(a > 1) 1 else a
      val bb = if(b < 0) 0 else if(b > 1) 1 else b
      positionColour(aa,bb,if(intensity > 1) 1 else intensity).getRGB
    }
  }

  }
