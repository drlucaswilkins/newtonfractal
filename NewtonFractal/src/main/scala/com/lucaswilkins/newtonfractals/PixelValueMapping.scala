package com.lucaswilkins.newtonfractals

/**
 * Trait that allows conversion between pixels and values in [0,1]
 */
trait PixelValueMapping {
  def xSize: Int
  def ySize: Int

  /**
   * Converts a pixel coordinate to [0,1]
   *
   * @param i pixel x
   * @param j pixel y
   * @return  a tuple of doubles in [0,1]x[0,1]
   */
  def indexValue(i: Int, j: Int): Tuple2[Double, Double] = (i.toDouble / xSize, j.toDouble / ySize)

  /**
   * Converts a value in [0,1] to a pixel
   *
   * @param a x value
   * @param b y value
   * @return a pixel index
   *
   */
  def pixelValue(a: Double, b: Double) = {
    val i = (a*xSize).toInt
    val j = (b*xSize).toInt
    (if(i < 0) 0 else if(i >= xSize) xSize-1 else i,
     if(j < 0) 0 else if(j >= ySize) ySize-1 else j)
  }
}
