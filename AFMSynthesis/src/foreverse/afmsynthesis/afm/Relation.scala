package foreverse.afmsynthesis.afm

abstract class Relation(parent : Feature, children : List[Feature])

case class Optional(parent : Feature, child : Feature) extends Relation(parent, List(child))
case class Mandatory(parent : Feature, child : Feature) extends Relation(parent, List(child))
case class MutexGroup(parent : Feature, children : List[Feature]) extends Relation(parent, children) {
  override def toString() = {
    "Mutex group : " + parent + " <- " + children.mkString("{", ",", "}")
  }
}
case class XorGroup(parent : Feature, children : List[Feature]) extends Relation(parent, children)
case class OrGroup(parent : Feature, children : List[Feature]) extends Relation(parent, children)
