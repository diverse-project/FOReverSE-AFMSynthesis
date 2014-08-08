package foreverse.afmsynthesis.algorithm

import foreverse.afmsynthesis.afm.Value

class BinaryImpliesExcludesConstraint(
	val value : Value,
	val implies : List[Value],
	val excludes : List[Value]
) {

  override def toString() = {
    value + " " + implies.mkString("{", ", ", "}") + ", " + excludes.mkString("{", ", ", "}")
  }
  
}