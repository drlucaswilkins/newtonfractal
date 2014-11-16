package com.lucaswilkins.newtonfractals

import java.awt.Component
import java.awt.image.BufferedImage
import java.io.File
import java.util.prefs.Preferences
import javax.imageio.ImageIO
import javax.swing.{JOptionPane, JFileChooser}
import javax.swing.filechooser.FileNameExtensionFilter

/**
 * This handles the file dialog and the saving of the image
 */
object image {
  val directoryKey = "DefaultSaveDirectory"

  /*
   * This part is to fairly safely handle setting a default directory
   */

  val prefs = Preferences.userRoot.node(getClass().getName)

  var _currentDirectory: File = {
    val putativeDirectory = prefs.get(directoryKey, System.getProperty("user.home"))
    val putativeFile = new File(putativeDirectory)
    val existingFile =
      if(putativeFile.exists()) {
        putativeFile
      } else {
        new File(System.getProperty("user.home"))
      }

    if(existingFile.isDirectory) {
      existingFile
    } else {
      existingFile.getParentFile()
    }

  }

  def currentDirectory = _currentDirectory
  def currentDirectory_=(file: File): Unit = if(file.exists) {
    _currentDirectory = if(file.isDirectory) file else file.getParentFile
    prefs.put(directoryKey, file.getAbsolutePath)
  }


  /*
   *  File chooser stuff
   */
  val fileChooser = new JFileChooser
  val filters = (("PNG Image", "png" :: Nil) ::
                 ("JPEG Image", "jpg" :: "jpeg" :: Nil) ::
                 ("Windows Bitmap", "bmp" :: Nil) :: Nil)
                  .map(x ⇒ new FileNameExtensionFilter(x._1, x._2:_*))
  filters.map(fileChooser addChoosableFileFilter _)
  fileChooser.setCurrentDirectory(currentDirectory)

  /**
   * Save a buffered image to file
   *
   * @param im     the buffered image
   * @param parent parent window for the file chooser
   */
  def saveBufferedImage(im: BufferedImage, parent: Component): Unit = {
    var retry = true

    case class RetryException(msg: String) extends Exception(msg)

    while (retry){

      retry = false

      val returnValue = fileChooser.showSaveDialog(parent)
      if (returnValue == JFileChooser.APPROVE_OPTION) {
        val selectedFile = fileChooser.getSelectedFile
        currentDirectory = selectedFile

        try {
          val imType =
            filters
              .find(_.accept(selectedFile))
              .getOrElse(throw RetryException("Unknown image file type."))
              .getExtensions
              .toList
              .apply(0)

          writeBufferedImage(im, selectedFile, imType)

        } catch {
          case msg: Throwable ⇒ {
            JOptionPane.showMessageDialog(
              parent,
              "Write Failed: " + msg.getMessage(),
              "Write Failed!",
              JOptionPane.ERROR_MESSAGE)

            msg match {
              case RetryException(_) ⇒ retry = true
              case _ ⇒
            }
          }
        }
    }
  }


  }

  /*
   *  Actual writing to file
   */
  def writeBufferedImage(im: BufferedImage, file: File, imType: String): Unit = {
      ImageIO.write(im, imType, file)
  }

}
