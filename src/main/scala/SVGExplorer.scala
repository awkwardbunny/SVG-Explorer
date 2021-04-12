package svgexplorer

import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
import scalafx.scene.Scene
import scalafx.scene.canvas.{Canvas, GraphicsContext}
import scalafx.scene.control.{Menu, MenuBar, MenuItem}
import scalafx.scene.input.{KeyCode, KeyCodeCombination, KeyCombination}
import scalafx.scene.layout.{HBox, VBox}
import scalafx.scene.paint.Color
import scalafx.scene.shape.Path
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
  val canvas = new scalafx.scene.Group() //new Canvas(3000, 3000)

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
    val viewBox = (svg.attributes.get("viewBox") getOrElse "0 0 100 100").toString.split("[,\\s+]").map(x => x.toDouble)
    canvas.children = svg.paths
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
