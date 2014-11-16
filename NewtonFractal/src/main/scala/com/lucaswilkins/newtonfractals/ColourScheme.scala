package com.lucaswilkins.newtonfractals

import java.awt.Color
import java.awt.image.BufferedImage

/**
 * Created by lucas on 16/11/2014.
 */
trait ColourScheme extends PixelValueMapping{

  /**
   * Get the colour associated with a position in the plane
   *
   * @param a a number in [0,1]
   * @param b a number in [0,1]
   * @return  a colour
   */
  def positionColour(a: Double, b: Double, intensity: Double = 1): Color

  val data = new BufferedImage(xSize,ySize,BufferedImage.TYPE_INT_RGB)

  for(i ← 0 until xSize) {
    for(j ← 0 until ySize) {
      val (a,b) = indexValue(i,j)
      data.setRGB(i, j, positionColour(a,b).getRGB)
    }
  }
  def byIndex(i: Int, j: Int) = data.getRGB(i,j)

  def bySolution(a: Double, b: Double, intensity: Double): Int = {
    val (i,j) = pixelValue(a,b)
    val color = new Color(data.getRGB(i,j))
    val p = if(intensity > 1) 1 else intensity
    val p255 = 255*p
    val q = 1-p

    val red: Int = ((color.getRed() * q) + p255).toInt
    val green: Int = ((color.getGreen() * q) + p255).toInt
    val blue: Int = ((color.getBlue() * q) + p255).toInt

    (new Color(red,green,blue)).getRGB

  }
}
