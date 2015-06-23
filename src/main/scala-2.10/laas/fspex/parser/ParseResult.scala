package laas.fspex.parser

import laas.fspex.model.WeightedGraph

/**
 * Created by Ulrich Matchi Aïvodji on 23/06/2015.
 */
case class ParseResult(graph:WeightedGraph) {
}
trait GraphFileSet {
  val name:String
  def parse():ParseResult
}