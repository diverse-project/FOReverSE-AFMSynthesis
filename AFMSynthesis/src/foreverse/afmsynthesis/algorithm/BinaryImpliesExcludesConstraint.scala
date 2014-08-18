package foreverse.afmsynthesis.algorithm

import foreverse.afmsynthesis.afm.constraint.Variable

class BinaryImpliesExcludesConstraint(
	val value : Variable,
	val implies : List[Variable],
	val excludes : List[Variable]
) {

  override def toString() = {
    value + " " + implies.mkString("{", ", ", "}") + ", " + excludes.mkString("{", ", ", "}")
  }
  
}