package com.lucaswilkins.newtonfractals

import java.awt.event.{MouseListener, MouseEvent}


/**
 * Neatened version of mouse listener for just mouse releases
 *
 * @param onRelease
 */
class MouseReleasedListener(onRelease: MouseEvent ⇒ Unit) extends MouseListener {
    def mouseClicked(e: MouseEvent): Unit = {}

    def mouseEntered(e: MouseEvent): Unit = {}

    def mouseExited(e: MouseEvent): Unit = {}

    def mousePressed(e: MouseEvent): Unit = {}

    def mouseReleased(e: MouseEvent): Unit = onRelease(e)

  }

