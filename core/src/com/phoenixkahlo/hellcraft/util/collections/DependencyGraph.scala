package com.phoenixkahlo.hellcraft.util.collections

import scala.collection.immutable.HashSet

case class DependencyGraph(managing: Set[ResourceNode] = Set.empty) {

  type Node = ResourceNode

  def ++(nodes: Seq[Node]): DependencyGraph =
    DependencyGraph(managing ++ nodes)

  def --(nodes: Seq[Node]): DependencyGraph =
    DependencyGraph(managing -- nodes)

  def garbage(roots: Seq[Node]): Set[Node] = {
    // depth first search starting with roots
    def connected(start: Node, found: Set[Node]): Set[Node] =
      start.dependencies.filterNot(found contains).foldLeft(found)({ case (a, n) => a ++ connected(n, a) })
    val keep = roots.foldLeft(roots.to[Set])({ case (a, n) => a ++ connected(n, a) })
    managing -- keep
  }

}

trait ResourceNode {

  def dependencies: Seq[ResourceNode]

  def dispose(): Unit

}