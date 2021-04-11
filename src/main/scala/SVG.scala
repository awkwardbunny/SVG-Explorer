package svgexplorer

import java.io.{File, FileReader}

import javax.xml.parsers.SAXParserFactory
import javax.xml.stream.{XMLInputFactory, XMLStreamConstants}

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
}
