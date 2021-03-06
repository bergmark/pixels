import java.awt.Color
import java.io.File
import java.net.URI
import javax.imageio.ImageIO

import scala.collection.immutable.HashMap
import scala.io._
import scala.swing._
import scala.swing.event.MouseMoved

case class Job (filePath : String, plate : String) {

}

object IO {
  import Strings._
  import Colors._

  def readColors(path: String): Array[Array[Color]] = {
    val fp = getFile(path)
    println("Reading " + fp)
    val img = ImageIO.read(new File(fp))
    val raster = img.getData
    val h = raster.getHeight
    val w = raster.getWidth
    println("height = " + h + ", width = " + w)
    Array.tabulate(w, h) { (x, y) => new Color(img.getRGB(x, y))}
  }

  def readPHColors: HashMap[Color, String] = {
    val source = Source.fromFile(getFile("colors.txt"))
    val lines = source.mkString
    source.close()
    lines.split("\n").map { v => v.split("=")}.map { v => (v(0), Color.decode("#" + v(1)))}.foldLeft(new HashMap[Color, String]) { case (h, (s, c)) => h + (c -> s)}
  }

  def getFile (name : String) : URI = {
    Main.resourceFromClassloader(name).toURI
  }

  def printBuildPlan(phColors : HashMap[Color,String], img : Array[Array[Color]]) : Array[Array[Color]] = {
    countColors(img).foreach { case (c:Color, i:Int) =>
      println(
        phColors(c)
          + " (" + padL(c.getRed.toString  ,3)
          + ","  + padL(c.getGreen.toString,3)
          + ","  + padL(c.getBlue.toString ,3)
          + ")"
          + ", "
          + padL(i.toString,4) + "px, "
          + padR((i.toFloat/140).toString,11)
          + " ~ "
          + Math.ceil(i.toFloat/140) + " squares"
      )
    }
    println("total pixels = " + countColors(img).foldLeft(0) { case (acc,(_,i)) => acc + i })
    img
  }

}

object Config {
  val jobs : HashMap[String,Job] = HashMap(
      "bulbasaur" -> Job("001-bulbasaur-40x50.png", "big")
    , "growlite"  -> Job("058-growlite-48x48.png", "small")
    , "dragonite" -> Job("149-dragonite-unsized.png", "big")
    , "walter"    -> Job("walter.png", "small")
  )
  val plates = HashMap[String,Tuple2[Int,Int]]("small" -> Tuple2(24, 24), "big" -> Tuple2(40, 50), "biglandscape" -> Tuple2(50,40))
}

object Colors {

  val yuvMatrix : Array[Array[Double]] = Array(Array(0.299,0.587,0.114),Array(-0.14713,-0.28886,0.436),Array(0.615,-0.51499,-0.10001))

  def toYuv(c : Color) : Array[Double] = {
    val r : Array[Double] = Array(c.getRed.toDouble, c.getGreen.toDouble, c.getBlue.toDouble)
    Matrix.multiply(yuvMatrix, r)
  }

  def yuvDistance(ca : Color, cb : Color) : Double = {
    val a = toYuv(ca)
    val b = toYuv(cb)
    Math.abs(a(0) - b(0)) + Math.abs(a(1) - b(1)) + Math.abs(a(2) - b(2))
  }

  def findClosestColorYUV(c : Color, cs : Array[Color]) : Color = {
    cs.minBy { c2 => yuvDistance(c, c2) }
  }

  def findClosestColorRGB(c : Color, cs: Array[Color]) : Color = {
    cs.minBy { c2 => Math.abs(c.getRed - c2.getRed) + Math.abs(c.getGreen - c2.getGreen) + Math.abs(c.getBlue - c2.getBlue) }
  }

  def makeYUV(phColors : HashMap[Color,String], img : Array[Array[Color]]) : Array[Array[Color]] = {
    img.map { r => r.map { c => findClosestColorYUV(c, phColors.keys.toArray) } }
  }

  def limitColors(maxColors : Int, img : Array[Array[Color]]) : Array[Array[Color]] = {
    val count = countColors(img)
    val keptColors : Array[Color] = count.toArray.sortBy(t => -t._2).take(maxColors).map(t => t._1)
    img.map { r =>
      r.map { c =>
        if (keptColors.contains(c)) {
          c
        } else {
          findClosestColorYUV(c, keptColors)
        }
      }
    }
  }

  def makeRGB(phColors : HashMap[Color,String], img : Array[Array[Color]]) : Array[Array[Color]] = {
    img.map { r => r.map { c => findClosestColorRGB(c, phColors.keys.toArray) } }
  }

  def countColors(img : Array[Array[Color]]) : HashMap[Color,Int] = {
    img.flatten.foldLeft(new HashMap[Color,Int]) { (h, c) => h + (c -> h.get(c).map(i => i + 1).getOrElse(1)) }
  }


}

object Strings {


  def padL(s : String, n : Int) : String = {
    var res = s
    while (res.length < n) {
      res = " " + res
    }
    res
  }

  def padR(s : String, n : Int) : String = {
    var res = s
    while (res.length < n) {
      res = res + " "
    }
    res
  }


}

class DataPanel(phColors : HashMap[Color,String], log : Log, data : Array[Array[Color]], job : Job, pixelSize : Int) extends Panel {
  preferredSize      = new Dimension(600, 600)
  val w              = pixelSize
  val h              = pixelSize
  val grid           = 1
  val plateSepWidth  = 4
  val plateSepHeight = 4
  val plateWidth     = Config.plates(job.plate)._1
  val plateHeight    = Config.plates(job.plate)._2
  val rows           = data.length
  val cols           = data(1).length
  val totalWidth     = cols*w + ((grid * cols) - 1) + (Math.floor(cols/plateWidth).toInt * plateSepWidth)

  override def paintComponent(g : Graphics2D) {
    g.setColor(new Color(0,0,0))
    g.fillRect(0, 0, size.width, size.height)

    val lastSelectedColor : Option[Color] = lastLogged.map(getColor)

    for {
      x <- 0 until rows
      y <- 0 until cols
    } {
      val clr = data(x)(y)
      g.setColor(clr)
      val gridX: Int = Math.floor(x.toFloat / plateWidth.toFloat).toInt * plateSepWidth
      val gridY: Int = Math.floor(y.toFloat / plateHeight.toFloat).toInt * plateSepHeight

      val x0 = x * w + x * grid + gridX
      val y0 = y * h + y * grid + gridY
      g.fillRect(x0, y0, w, h)
      val isHoveredColor : Boolean = lastSelectedColor.exists(lsc => lsc == clr)
      if (isHoveredColor) {
        g.setColor(Color.green)
        g.drawRect(x0-1, y0-1, w+1, h+1)
      }
    }
  }

  var lastLogged : Option[Cell] = None

  listenTo(mouse.moves)
  reactions += {
    case e: MouseMoved =>
      val mc = getCell(e.point)
      mc.map { c =>
        val clr = getColor(c)
        val gc = gridPosition(c)
        val cellInGrid = cellInGridPosition(c)
        val clrString =  "#"+Integer.toHexString(clr.getRGB).substring(2).toUpperCase
        if (!someEquals(lastLogged, Some(c))) {
          lastLogged = Some(c)
          log.write(/*e.point.x + ", " + e.point.y + " => " + */ gc.toString + ", " + cellInGrid.toString + ", color: " + phColors.get(clr).getOrElse("N/A") + ", " + clrString)
          repaint()
        }
      }
  }

  def gridPosition (c : Cell) : GridCell = {
    val x = Math.floor(c.x / plateWidth).toInt
    val y = Math.floor(c.y / plateHeight).toInt
    GridCell(x, y)
  }

  def cellInGridPosition (c : Cell) : Cell = {
    val gc = gridPosition(c)
    Cell(c.x - gc.x * plateWidth, c.y - gc.y * plateHeight)
  }

  def someEquals(a : Option[Cell], b : Option[Cell]) : Boolean = Tuple2(a,b) match {
    case Tuple2(Some(a),Some(b)) => a.equals(b)
    case Tuple2(None, None)      => true
    case _                       => false
  }

  def getColor(c : Cell) : Color = {
    data(c.x)(c.y)
  }

  def getCell(p : Point) : Option[Cell] = {
    val x = p.x / (w + grid)
    val y = p.y / (h + grid)
    if(x < 0 || y < 0 || x >= rows || y >= cols) {
      None
    } else {
      Some(Cell(x, y))
    }
  }
}

case class Cell(x : Int, y : Int) {
  override def toString : String = {
    "Cell(" + Strings.padL(x.toString,3) + ", " + Strings.padL(y.toString,3) + ")"
  }
  override def equals(o : Any) = o match {
    case c:Cell => x == c.x && y == c.y
    case _ => false
  }
}

case class GridCell(x : Int, y : Int) {
  override def toString : String = {
    "Grid(" + Strings.padL(x.toString,3) + ", " + Strings.padL(y.toString,3) + ")"
  }
  override def equals(o : Any) = o match {
    case c:Cell => x == c.x && y == c.y
    case _ => false
  }
}

class Log extends TextArea(rows=40, columns=40) {
  preferredSize = new Dimension(600,600)
  def write(s : String) {
    text = s + "\n" + text
  }
}

object Main extends SimpleSwingApplication {
  val pixelSize  = 13
  val phColors   = IO.readPHColors
  val job        = Config.jobs("growlite")
  val origColors = IO.readColors(job.filePath)
  val log        = new Log()
  val origImg    = new DataPanel(phColors, log, origColors, job, pixelSize)
  val yuvImg     = new DataPanel(phColors, log, IO.printBuildPlan(phColors, Colors.makeYUV(phColors, origColors)), job, pixelSize)
  def top        = new MainFrame {
    title    = "Pixels"
    contents = new GridPanel(2,2) {
      background    = new Color(0, 0,0)
      preferredSize = new Dimension(2000, 1400)
      contents     += yuvImg
      contents     += origImg
      contents     += log
//      contents     += new DataPanel(Data.makeRGB(data))
    }
  }
}
