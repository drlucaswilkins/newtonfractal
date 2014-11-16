package com.lucaswilkins.newtonfractal

import java.awt.Event
import java.awt.event.{ActionEvent, ActionListener, KeyEvent}
import javax.swing._

import com.lucaswilkins.newtonfractals.{util, image, MainPanel}


/**
 * Created by lucas on 15/11/2014.
 */
class MainWindow extends JFrame("Newton-Raphson Fractal Manipulator") {
  val w = 600
  val h = 600

  /*
   *  Look and feel
   */

  try {

    UIManager.setLookAndFeel("javax.swing.plaf.nimbus.NimbusLookAndFeel")

  } catch {
    case _: Throwable ⇒ println("Failed to set look and feel")
  }

  /*
   *   Menu bar
   */

  def run(f: Function0[Unit]) =
    new ActionListener(){
      def actionPerformed(e: ActionEvent) {
        f.apply
      }
  }

  val menu = new JMenuBar

  val file = new JMenu("File")
  file.setMnemonic(KeyEvent.VK_F)
  menu.add(file)

  val save = new JMenuItem("Save Image")
  save.setMnemonic(KeyEvent.VK_S)
  save.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_S, Event.CTRL_MASK))
  save.addActionListener(run {case _ ⇒ image.saveBufferedImage(panel.canvas,this)})
  file.add(save)
  file.addSeparator

  val exit = new JMenuItem("Exit")
  exit.setMnemonic(KeyEvent.VK_X)
  exit.addActionListener(run({case _ ⇒ System.exit(0) }))
  file.add(exit)

  val control = new JMenu("Control")
  control.setMnemonic(KeyEvent.VK_C)
  menu.add(control)

  val deleteNode = new JMenuItem("Remove selected control node")
  deleteNode.setMnemonic(KeyEvent.VK_D)
  deleteNode.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_DELETE.toChar))

  deleteNode.addActionListener(run{case _ ⇒ panel.deleteSelectedNode})

  control.add(deleteNode)

  val clearNode = new JMenuItem("Clear all control nodes")
  clearNode.setMnemonic(KeyEvent.VK_A)
  clearNode.addActionListener(run {case _ ⇒ panel.deleteAllNodes})
  control.add(clearNode)

  control.addSeparator

  val showRoots = new JCheckBoxMenuItem("Show control nodes")
  showRoots.setMnemonic(KeyEvent.VK_N)
  showRoots.setSelected(true)
  showRoots.addActionListener(run{case _ ⇒ panel.showNodes = showRoots.isSelected})
  control.add(showRoots)

  val showPoly = new JCheckBoxMenuItem("Show data")
  showPoly.setMnemonic(KeyEvent.VK_P)
  showPoly.setSelected(false)
  showPoly.addActionListener(run {
    case _ ⇒ panel.polyDisplay.setVisible(showPoly.isSelected)
  })

  control.add(showPoly)

  setJMenuBar(menu)

  val colourSchemes = new JMenu("Colours")
  colourSchemes.setMnemonic(KeyEvent.VK_S)
  menu.add(colourSchemes)

  val help = new JMenu("Help")
  help.setMnemonic(KeyEvent.VK_H)
  menu.add(help)

  val website = new JMenuItem("Online Documentation")
  website.setMnemonic(KeyEvent.VK_D)
  website.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_F1, 0))
  website.addActionListener(run {case _ ⇒ util.launchWebsite("http://www.lucaswilkins.com/newtonfractal/")})
  help.add(website)

  /*
   *  Main panel
   */

  val ins = getInsets

  val panel = new MainPanel(w-(ins.left+ins.right), h-(ins.top-ins.bottom))

  /*
   *  Populate colour scheme menu
   */

  var schemeNameMenuMap: scala.collection.mutable.Map[String, JCheckBoxMenuItem]
    = scala.collection.mutable.Map[String, JCheckBoxMenuItem]()

  for (schemeName ← panel.colourSchemes.keys){
    val menuItem: JCheckBoxMenuItem = new JCheckBoxMenuItem(schemeName)
    menuItem.setSelected(schemeName == panel.defaultSchemeName)

    menuItem.addActionListener(run {case _ ⇒ {
        panel.setColourScheme(schemeName)
        schemeNameMenuMap.values.map(_ setSelected false)
        menuItem.setSelected(true)
      }
    })

    colourSchemes.add(menuItem)
    schemeNameMenuMap.put(schemeName, menuItem)
  }

  /*
   * Other basic swing stuff
   */

  add(panel)
  setSize(600,600)
  setResizable(false)
  setLocationRelativeTo(null)
  setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
  setVisible(true)


}

object MainWindow {

  def main(args: Array[String]): Unit = {
    new MainWindow
  }
}
