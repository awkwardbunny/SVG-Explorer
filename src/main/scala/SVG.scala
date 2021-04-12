package svgexplorer

import java.io.{File, FileReader}

import javax.xml.parsers.SAXParserFactory
import javax.xml.stream.{XMLInputFactory, XMLStreamConstants}
import scalafx.scene.paint.Color
import scalafx.scene.shape.{FillRule, Path, SVGPath}
import scalafx.scene.transform.{Scale, Transform, Translate}

import scala.xml._

object SVGParser {
  private val parf = SAXParserFactory.newInstance
  //    parf.setNamespaceAware(false)
  //    parf.setFeature("http://apache.org/xml/features/nonvalidating/load-dtd-grammar", false)
  parf.setFeature("http://apache.org/xml/features/nonvalidating/load-external-dtd", false)

  def apply(path: String): SVG = this(new File(path))
  def apply(file: File): SVG = this(new FileReader(file))
  def apply(freader: FileReader): SVG = {
    new SVG(XML.loadXML(new InputSource(freader), parf.newSAXParser))
  }
  def empty: SVG = {
    new SVG(XML.loadString("<svg> </svg>"))
  }
}

class SVG(val xml: Elem ) {
  val attributes = xml.attributes.map(md => (md.key -> md.value)).toMap
  override def toString = xml.toString

//  val node: scalafx.scene.Node
  val paths: List[SVGPath] = getPaths(xml, 0)

  private def getPaths(e: Node, depth: Int): List[SVGPath] = {
    e.child.map {
      case g @ Elem(_,"g",attr,_,_*) => {
        val transforms = "(\\S+)\\(([^)]*)\\)".r.findAllMatchIn(
          (attr.get("transform") getOrElse "").toString
        ).map( m => {
          val args = m.group(2).split("[,\\s]").map(x => x.toDouble).toSeq
          m.group(1) match {
            case "translate" => new Translate(args(0), args(1))
            case "scale" => new Scale(args(0), args(1))
//            case _ => ()
          }
        }).toList
        println(transforms)

        println(s"Found <g> at ${depth}"); List()
        val c = getPaths(g, depth+1)
        for(p <- c; t <- transforms) p.transforms.add(t)
        c.map(x => println(x))
        c
      }
      case Elem(_,"path",attr,_,_*) => List(new SVGPath{
        content = (attr.get("d") getOrElse "").toString
//        this.setFill(Color.Transparent)
//        this.setStroke(Color.Red)
//        this.setStrokeWidth(2.0)
      })
      case u @ Elem(_, tag, _,_,_*) => {
        println(s"Found tag ${tag} at ${depth}")
        getPaths(u, depth+1)
      }
      case u => getPaths(u, depth+1)
    }
  }.toList.flatten
}
