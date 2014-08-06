package foreverse.afmsynthesis.afm

abstract class Relation(val parent : Feature, val children : Set[Feature])

case class Optional(initParent : Feature, child : Feature) extends Relation(initParent, Set(child))
case class Mandatory(initParent : Feature, child : Feature) extends Relation(initParent, Set(child))
case class MutexGroup(initParent : Feature, initChildren : Set[Feature]) extends Relation(initParent, initChildren) {
  override def toString() = {
    "Mutex group : " + parent + " <- " + children.mkString("{", ",", "}")
  }
}
case class XorGroup(initParent : Feature, initChildren : Set[Feature]) extends Relation(initParent, initChildren)
case class OrGroup(initParent : Feature, initChildren : Set[Feature]) extends Relation(initParent, initChildren)
