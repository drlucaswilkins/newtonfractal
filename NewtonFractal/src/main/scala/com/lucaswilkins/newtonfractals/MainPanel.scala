package com.lucaswilkins.newtonfractals


import java.awt.event.MouseListener
import java.awt.{Font, Color, Graphics2D, Graphics}
import java.awt.image.BufferedImage
import java.awt.event.MouseEvent
import javax.swing.text.StyleConstants
import javax.swing.{BoxLayout, JTextArea, JLabel, JPanel}

import com.lucaswilkins.newtonfractals.complex.Complex
import com.lucaswilkins.newtonfractals.polynomial._
import com.lucaswilkins.newtonfractals.colourschemes._



/**
 * The panel containing the fractal
 */
class MainPanel(val xSize: Int, val ySize: Int)
  extends JPanel with PixelValueMapping {


  setLayout(new BoxLayout(this, BoxLayout.PAGE_AXIS))


  /*
   *  Basic elements
   */

  val canvas = new BufferedImage(xSize,ySize,BufferedImage.TYPE_INT_RGB)
  val controlNodes = new SelectableArray[ControlNode]()

  var iters = 10



  /*
   *  Colour schemes
   */
  val colourSchemes: Map[String, ColourScheme] = Map(
    ("Hues", new Hues(xSize, ySize)),
    ("Hues Two", new Hues2(xSize, ySize)),
    ("Sea", new BlueGreens(xSize, ySize)),
    ("Rhubarb and Custard", new Fire(xSize, ySize)),
    ("Raw data as RGB", new RawRGB(xSize, ySize))
    )

  val defaultSchemeName = "Hues"

  private var _colourArray = colourSchemes(defaultSchemeName)
  def setColourScheme(name: String) = try {

    _colourArray = colourSchemes.getOrElse(name, throw new Exception)

    setFractalColours

    repaint()
  } catch {
    case _: Throwable ⇒
      println("Failed to set colour scheme - this should not be possible")
  }


  /*
   *  Node visibility
   */
  private var _showNodes = true
  def showNodes = _showNodes
  def showNodes_=(s: Boolean): Unit = {
    _showNodes = s
    repaint()
  }

  /*
   *  Mouse listener
   */

  def onClick(e: MouseEvent): Unit = {
    if(e.getButton == MouseEvent.BUTTON1) {
      // Left click
      if (showNodes) {
        /*
         *   If within 10 pixels of a node, select the nearest one,
         *   else create a new one.
         */

        /* minBy requires non-empty list */
        if (controlNodes.length == 0) {
          addNode(e.getX, e.getY)
          repaint()
          return
        }

        val shortest =
          controlNodes
            .toList
            .map(_.getScreenPosition(xSize, ySize))
            .map(p ⇒ scala.math.pow(p._1 - e.getX, 2) + scala.math.pow(p._2 - e.getY, 2))
            .zipWithIndex
            .minBy(x ⇒ x._1)

        if (shortest._1 < 10 * 10) {
          controlNodes.select(shortest._2)
        } else {
          addNode(e.getX, e.getY)
        }

        repaint()
      }
    }

  }

  addMouseListener(new MouseClickListener(onClick))

  /*
   *  Mouse motion listener
   */


  var hasDragged = false // For choosing when to refresh

  def onDrag(e: MouseEvent): Unit = {
    // is there a selected one, and is it within, say, 20 pixels

    controlNodes.getSelected.map (selection ⇒ {
      val screenPos = selection.getScreenPosition(xSize, ySize)
      val distanceSq = scala.math.pow(screenPos._1 - e.getX, 2) + scala.math.pow(screenPos._2 - e.getY, 2)

      if(distanceSq < 30*30) {
        selection.setFromScreenPosition(xSize, ySize, e.getX, e.getY)
        hasDragged = true
        repaint()
      }
    })

  }

  addMouseMotionListener(new MouseDragListener(onDrag))

  /* Refresh the fractal when the drag finishes */
  def onRelease(e: MouseEvent): Unit = {
    if(hasDragged){
      recalculatePolynomial
      setFractalColours
      repaint()
    }
    hasDragged = false
  }

  addMouseListener(new MouseReleasedListener(onRelease))

  /*
   * Information about the current fractal
   */

  def info: String = (
    "Data for current polynomial\n" ::
    "===========================\n\n" ::
    "Polynomial Roots: \n" ::
    controlNodes.toList.map("  "+_.asComplex+"\n") :::
    "\nCurrent polynomial: " :: poly.toString ::
    s"\n\nIterations (not currently modifiable): $iters" :: Nil
    ).mkString("")



  val areaFont: Font = new javax.swing.plaf.FontUIResource("Monospaced",Font.PLAIN,12)

  val polyDisplay = new JTextArea("No polynomial specified. Try clicking!")
  polyDisplay.setEditable(false)
  polyDisplay.setLineWrap(true)
  polyDisplay.setWrapStyleWord(true)
  polyDisplay.setFont(areaFont)
  polyDisplay.setVisible(false)
  add(polyDisplay)

  /*
   * The polynomial and its representation
   */

  private var _poly = Polynomial(0::Nil)
  private var solver = new Newton(_poly, iters)
  def poly = _poly
  def poly_=(p: Polynomial) {
    _poly = p
    polyDisplay.setText(info)
    solver = new Newton(_poly, iters)
    setFractalColours
    repaint()
  }


  /*
   *   Main drawing bit
   */

  setFractalColours

  private val xvals = (0 until xSize).toArray
  private val yvals = (0 until ySize).toArray
  def setFractalColours: Unit = {

    if(poly.order < 1) {
      for (i ← 0 until xSize) {
        for (j ← 0 until ySize) {
          val (a, b) = indexValue(j,i)
          canvas.setRGB(i, j, _colourArray.bySolution(a,b,0))
        }
      }
    } else {
      val data =
        xvals.map(i ⇒ {
          yvals.map(j ⇒ {
            val (a, b) = indexValue(j,i)
            val (root, amount) = solver.solve(Complex(a,b))
            _colourArray.bySolution(root.re, root.im,20*amount)
          })
        }).flatten

      canvas.getRaster.setDataElements(0,0,xSize,ySize,data)
    }

  }


  /**
   *  Recalculate the polynomial from the control nodes
   */
  def recalculatePolynomial: Unit = {
    val roots = controlNodes.toList.map(_.asComplex)
    poly = Polynomial.fromRoots(roots)
  }

  /**
   * Delete the select node
   */
  def deleteSelectedNode = {


    if(showNodes) {

      controlNodes.deleteSelected

      recalculatePolynomial

      repaint()
    }
  }

  def deleteAllNodes = {

    //if(showNodes) { // Hard to say whether this should work when nodes are hidden!
      controlNodes.deleteAll
      recalculatePolynomial
      repaint()
    //}
  }

  /**
   * Add a node
   *
   * @param i x position on image
   * @param j y position on screen
   */
  def addNode(i: Int, j: Int): Unit = {


    if ((i < 0) || (j < 0) || (i >= xSize) || (j >= ySize)) {
      controlNodes += new ControlNode(0.5, 0.5)
    } else {
      val (a, b) = indexValue(i, j)
      controlNodes += new ControlNode(a, b)
    }

    recalculatePolynomial

    repaint()

  }


  override def paintComponent(g: Graphics): Unit = {
    super.paintComponent(g)

    val g2 = g.asInstanceOf[Graphics2D]
    g2.drawImage(canvas, null, null)

    if(showNodes)
      controlNodes.withSelection.map {
        case (controlNode, selected) ⇒ {
          val (i,j) = controlNode.getScreenPosition(xSize,ySize)
          drawCrossAt (g, i, j, selected)
        }
      }


  }

  /**
   * Draw a cross
   *
   * @param g         The graphics to draw to
   * @param x         x position
   * @param y         y position
   * @param selected  Should it the selected version?
   */
  def drawCrossAt(g: Graphics, x: Int, y: Int, selected: Boolean = false) = {

    if(selected){

      g.setColor(Color.white)
      g.fillRect(x-5, y-1, 11, 3)
      g.fillRect(x-1, y-5, 3, 11)

      g.setColor(Color.black)
      g.drawLine(x-5, y, x+5, y)
      g.drawLine(x, y-5, x, y+5)

    } else {

      g.setColor(Color.black)
      g.fillRect(x-5, y-1, 11, 3)
      g.fillRect(x-1, y-5, 3, 11)

      g.setColor(Color.white)
      g.drawLine(x-5, y, x+5, y)
      g.drawLine(x, y-5, x, y+5)

    }
  }
}
