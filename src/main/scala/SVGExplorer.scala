package svgexplorer

import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
import scalafx.scene.Scene
import scalafx.scene.canvas.{Canvas, GraphicsContext}
import scalafx.scene.control.{Menu, MenuBar, MenuItem}
import scalafx.scene.input.{KeyCode, KeyCodeCombination, KeyCombination}
import scalafx.scene.layout.{HBox, VBox}
import scalafx.scene.paint.Color
import scalafx.stage.{FileChooser, Stage, Window}
import scalafx.stage.FileChooser.ExtensionFilter

import scala.xml._

object SVGExplorer extends JFXApp {

  stage = new PrimaryStage {
    implicit val w = this
    val explorer = new SVGExplorer

    title = "SVG Explorer"
    scene = explorer.scene
  }
}

class SVGExplorer(implicit w: Window) {

  private var svg = SVGParser.empty
  val canvas = new Canvas(3000, 3000)

  val shortcuts = Map(
    new KeyCodeCombination(KeyCode.O, KeyCombination.ControlDown) -> (() => openAction),
    new KeyCodeCombination(KeyCode.T, KeyCombination.ControlDown) -> (() => draw),
    new KeyCodeCombination(KeyCode.X, KeyCombination.ControlDown) -> (() => w.asInstanceOf[Stage].close())
  )

  val scene = new Scene(new VBox{
    children = List(
      createMenus,
      canvas
    )
  }, 500, 500) {
    for ((kc, action) <- shortcuts) this.accelerators.put(kc, new Runnable { override def run = action() })
  }

  private def openAction = {
    val f = new FileChooser {
      title = "Open SVG Image"
      selectedExtensionFilter = new ExtensionFilter("SVG Files", "*.svg")
      extensionFilters.addAll(
        new ExtensionFilter("SVG Files", "*.svg")
      )
    }.showOpenDialog(w)

    if(f != null){
      svg = SVGParser(f)
      draw
    }
  }

  private def draw = {
    val gc = canvas.graphicsContext2D
    gc.clearRect(0, 0, canvas.getWidth, canvas.getHeight)

    gc.setStroke(Color.Black)
    gc.setFill(Color.Red)
    gc.setLineWidth(1.0)

    val viewBox = (svg.attributes.get("viewBox") getOrElse "0 0 100 100").toString.split("[,\\s+]").map(x => x.toDouble)
//    canvas.width = viewBox(2)
//    canvas.height = viewBox(3)
//    gc.fillRect(viewBox(0), viewBox(1), viewBox(2), viewBox(3))

    val transformRegex = "(\\S+)\\(([^)]*)\\)".r
    svg.xml.child map {
      case g @ Elem(_,"g",attr,_,_*) => {
        val transform = transformRegex.findAllMatchIn(
          (attr.get("transform") getOrElse "").toString
        ).map( m => (
          m.group(1) -> m.group(2).split("[,\\s]").map(x => x.toDouble).toSeq)
        ).toMap

        def translate: ((Double, Double)) => (Double, Double) = (coords: (Double, Double)) => (coords._1 + transform("translate")(0), coords._2 + transform("translate")(1))
        def scale: ((Double, Double)) => (Double, Double) = (coords: (Double, Double)) => (coords._1 * transform("scale")(0), coords._2 * transform("scale")(1))
        val xforms = Map(
          "translate" -> translate,
          "scale" -> scale
        )

        g.child map {
          case Elem(_,"path",attr,_,_*) => {
            gc.beginPath()
            val spaces = (attr.get("d") getOrElse "").toString.split("(?=[z])").mkString(" ")
            doPath(gc, spaces, xforms, 0.0, 0.0)
          }
//          case t => println("Unknown tag2: " + t.label)
          case _ =>
        }
      }
//      case pcd @ PCData(text) => println(text)
//      case t => println("Unknown tag: " + t.label)
      case _ =>
    }
  }

  val letterRegex = """[a-zA-Z]""".r

  def doPath(gc: GraphicsContext, path: String, xform: Map[String,((Double, Double))=>(Double,Double)], cx: Double, cy: Double): Unit = {
    path match {
      case s"M${x} ${y} ${extra}" => {
//        println(s"MOVE ${extra}")
        val nc = xform("translate")(xform("scale") (x.toDouble, y.toDouble))
        gc.moveTo(nc._1, nc._2)
        extra(0) match {
          case letterRegex(_*) => doPath(gc, extra, xform, nc._1, nc._2)
          case _ => doPath(gc, "L"+extra, xform, nc._1, nc._2)
        }
      }
      case s"m${x} ${y} ${extra}" => {
//        println(s"MOVE ${extra}")
        val nc = xform("scale") (x.toDouble, y.toDouble)
        gc.moveTo(nc._1+cx, nc._2+cy)
        extra(0) match {
          case letterRegex(_*) => doPath(gc, extra, xform, nc._1+cx, nc._2+cy)
          case _ => doPath(gc, "L"+extra, xform, nc._1+cx, nc._2+cy)
        }
      }
      case s"c${x1} ${y1} ${x2} ${y2} ${dx} ${dy} ${extra}" => {
//        println(s"cubic ${extra}")
        val one = xform("scale") (x1.toDouble, y1.toDouble)
        val two = xform("scale") (x2.toDouble, y2.toDouble)
        val del = xform("scale") (dx.toDouble, dy.toDouble)
        gc.bezierCurveTo(one._1+cx, one._2+cy, two._1+cx, two._2+cy, del._1+cx, del._2+cy)
        extra(0) match {
          case letterRegex() => doPath(gc, extra, xform, del._1+cx, del._2+cy)
          case _ => doPath(gc, "c"+extra, xform, del._1+cx, del._2+cy)
        }
      }
      case s"l${x1} ${y1} ${extra}" => {
//        println(s"line ${extra}")
        val nc = xform("scale") (x1.toDouble, y1.toDouble)
        gc.lineTo(nc._1+cx, nc._2+cy)
        extra(0) match {
          case letterRegex() => doPath(gc, extra, xform, nc._1+cx, nc._2+cy)
          case _ => doPath(gc, "l"+extra, xform, nc._1+cx, nc._2+cy)
        }
      }
      case s"L${x1} ${y1} ${extra}" => {
//        println(s"LINE ${extra}")
        val nc = xform("translate")(xform("scale") (x1.toDouble, y1.toDouble))
        gc.lineTo(nc._1, nc._2)
        extra(0) match {
          case letterRegex() => doPath(gc, extra, xform, nc._1, nc._2)
          case _ => doPath(gc, "L"+extra, xform, nc._1, nc._2)
        }
      }
      case s"z ${extra}" => {
//        println("FIN")
//        gc.fill ()
        gc.stroke ()
        gc.closePath()
        doPath(gc, extra, xform, cx, cy)
      }
      case "z" => {
//        println("FIN")
        gc.fill()
        gc.stroke()
        gc.closePath()
      }
    }
  }

  def createMenus = new MenuBar {
    menus = List(
      new Menu("_File") {
        mnemonicParsing = true
        items = List(
          new MenuItem("_Open SVG") { onAction = _ => openAction },
          new MenuItem("_Exit") { onAction = _ => w.asInstanceOf[Stage].close() }
        )
      },
      new Menu("_Test") {
        mnemonicParsing = true
        items = List(
          new MenuItem("_Print") { onAction = _ => println(svg) },
          new MenuItem("_Draw") { onAction = _ => draw }
        )
      }
    )
  }
}
