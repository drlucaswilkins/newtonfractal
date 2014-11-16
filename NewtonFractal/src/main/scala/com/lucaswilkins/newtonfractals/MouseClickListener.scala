package com.lucaswilkins.newtonfractals

import java.awt.event.MouseListener
import java.awt.event.MouseEvent

/**
 * Neatened version of mouse listener for just clicks
 *
 * @param onClick
 */
class MouseClickListener(onClick: MouseEvent ⇒ Unit) extends MouseListener {
  def mouseClicked(e: MouseEvent): Unit = onClick(e)

  def mouseEntered(e: MouseEvent): Unit = {}

  def mouseExited(e: MouseEvent): Unit = {}

  def mousePressed(e: MouseEvent): Unit = {}

  def mouseReleased(e: MouseEvent): Unit = {}

}
