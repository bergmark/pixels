import java.awt.Color
import java.io.File
import java.net.URI
import javax.imageio.ImageIO

import scala.collection.immutable.HashMap
import scala.swing._
import scala.io._

object Data {
  def make : Array[Array[Color]] = {
    val fps : Array[String] = Array(
        "001-bulbasaur-40x50"
      , "058-growlite-48x48"
      , "149-dragonite-unsized"
      )
    val fp = Main.resourceFromClassloader(fps(1) + ".png").toURI()
    println("Reading " + fp)
    val img = ImageIO.read(new File(fp))
    val raster = img.getData
    val h = raster.getHeight
    val w = raster.getWidth
    println("height = " + h + ", width = " + w)
    Array.tabulate(w, h) { (x, y) => new Color(img.getRGB(x, y))}
  }

  def getFile (name : String) : URI = {
    Main.resourceFromClassloader(name).toURI
  }


  def readPHColors : HashMap[Color,String] = {
    val source = Source.fromFile(getFile("colors.txt"))
    val lines = source.mkString
    source.close
    lines.split("\n").map { v => v.split("=") }.map { v => (v(0), Color.decode("#" + v(1))) }.foldLeft(new HashMap[Color,String]) { case (h,(s,c)) => h + (c -> s) }
  }

  val phColors = readPHColors

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

  def findClosestColor(c : Color, cs : Array[Color]) : Color = {
    cs.minBy { c2 => yuvDistance(c, c2) }
  }

  def findClosestColorRGB(c : Color, cs: Array[Color]) : Color = {
    cs.minBy { c2 => Math.abs(c.getRed - c2.getRed) + Math.abs(c.getGreen - c2.getGreen) + Math.abs(c.getBlue - c2.getBlue) }
  }

  def g(c:Color,i:Int) {
  }

  def padL(s : String, n : Int) : String = {
    var res = s;
    while (res.length < n) {
      res = " " + res;
    }
    res
  }
  def padR(s : String, n : Int) : String = {
    var res = s;
    while (res.length < n) {
      res = res + " ";
    }
    res
  }

  def make(img : Array[Array[Color]]) : Array[Array[Color]] = {
    val r : Array[Array[Color]] = img.map { r => r.map { c => findClosestColor(c, phColors.keys.toArray) } }
    countColors(r).foreach { case (c:Color, i:Int) =>
      println(
          phColors(c)
        + " (" + padL(c.getRed.toString  ,3)
        + ","  + padL(c.getGreen.toString,3)
        + ","  + padL(c.getBlue.toString ,3)
        + ")"
        + ", "
        + padL(i.toString,4) + "px, "
        + padR(((i.toFloat)/140).toString,11)
        + " ~ "
        + Math.ceil((i.toFloat)/140) + " squares"
        )
    }
    println("total pixels = " + countColors(r).foldLeft(0) { case (acc,(_,i)) => acc + i })
    r
  }
  def makeRGB(img : Array[Array[Color]]) : Array[Array[Color]] = {
    img.map { r => r.map { c => findClosestColorRGB(c, phColors.keys.toArray) } }
  }

  def countColors(img : Array[Array[Color]]) : HashMap[Color,Int] = {
    img.flatten.foldLeft(new HashMap[Color,Int]) { (h, c) => h + (c -> h.get(c).map(i => i + 1).getOrElse(1)) }
  }
}

class DataPanel(data : Array[Array[Color]]) extends Panel {
  override def paintComponent(g : Graphics2D) {
    val w = 4
    val grid = 0
    g.setColor(new Color(0,0,0))
    g.fillRect(0, 0, size.width, size.height)
    for {
      x <- 0 until data.length
      y <- 0 until data(1).length
    } {
      g.setColor(data(x)(y))
      g.fillRect(x*w + x*grid, y*w + y*grid, w, w)
    }
  }
}

object Main extends SimpleSwingApplication {
  val data = Data.make
  def top  = new MainFrame {
    title    = "Pixels"
    contents = new GridPanel(2,2) {
      background    = new Color(0, 0,0)
      preferredSize = new Dimension(400, 400)
      contents     += new DataPanel(data)
      contents     += new DataPanel(Data.make(data))
      contents     += new DataPanel(Data.makeRGB(data))
    }
  }
}
