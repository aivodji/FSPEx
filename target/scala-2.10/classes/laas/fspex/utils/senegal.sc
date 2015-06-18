import scala.io.Source
import scala.xml
import scala.xml.MetaData
import scala.xml._
import scala.xml.XML
import scala.xml.pull._


object parser{

  val path="/Users/administrateur/OpenData/senegal/senegal.kml"
  val source = Source.fromFile(path).mkString

  /*val data=XML.load(path)

  val document=data.child.filter(N=>N.label=="Document")
  val p=document \\ "Placemark"
  val placemarks=p.map(N => (N \ "@description"))




  //val osm=x.child.filter(N=>N.label=="node")
  //val nodes=osm.map( N =>  ( N.attribute("id"), (N.attribute("lat"),N.attribute("lon")) ) )

  println("----------------------")

  placemarks foreach println*/

  val yo=XML.loadString(source)

  val placemark1=(yo \\ "Document" \\ "Placemark" \ "@name").text

  println(placemark1)


}