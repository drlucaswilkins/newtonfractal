package com.lucaswilkins.newtonfractals

import scala.collection.mutable.ArrayBuffer

/**
 * Created by lucas on 15/11/2014.
 */
class SelectableArray[T] extends ArrayBuffer[T] {
  var _selection: Option[Int] = None
  def selection = _selection
  def select(x: Int) = _selection = Some(x)
  def selectNone = _selection = None

  override def -=(x: T) = {
    super.-=(x)
    _selection = _selection.map(x ⇒ if (x < length) x else length-1)
    this
  }

  /**
   * Delete everything, select nothing
   */

  def deleteAll = {
    remove(0,length)
    selectNone
  }


  /**
   * Delete the currently selected item
   */
  def deleteSelected = {
    selection.map(remove(_))
    _selection = _selection
      .map(x ⇒ if (x < length) x else length-1)

    selection match {
      case Some(x) if (x < 0) ⇒ selectNone
      case _ ⇒
    }

  }

  /**
   * Option, if there's a currently selected item return it
   *
   * @return the currently selected item
   */
  def getSelected: Option[T] = selection.map(apply(_))

  /**
   * Contents zipped with a flag for whether it is selected
   */
  def withSelection =
    toList.zip(
      (0 until length)
        .map(_ == selection.getOrElse(-1)))

}
