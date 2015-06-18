package laas.fspex.parser

import laas.fspex.model._

/**
 * Created by Ulrich Matchi Aïvodji on 18/06/2015.
 */

case class ParseResult(graph:MutableGraph,namedLocations:NamedLocations,namedWays:NamedWays) {
  def merge(other:ParseResult) = {
    ParseResult(MutableGraph.merge(graph,other.graph),
      namedLocations.mergeIn(other.namedLocations),
      namedWays.mergeIn(other.namedWays))
  }
}

trait GraphFileSet {
  val name:String

  def parse():ParseResult
}