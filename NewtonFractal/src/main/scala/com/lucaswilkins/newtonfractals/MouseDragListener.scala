package com.lucaswilkins.newtonfractals

import java.awt.event.{MouseEvent, MouseMotionListener}

/**
 * Neatened for just dragging
 */

class MouseDragListener(f: MouseEvent ⇒ Unit) extends MouseMotionListener {
  override def mouseDragged(e: MouseEvent): Unit = f(e)

  override def mouseMoved(e: MouseEvent): Unit = {}
}
