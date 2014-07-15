package foreverse.afmsynthesis.afm

class BinaryImplicationConstraint(
	val value : Value,
	val implies : List[Value],
	val excludes : List[Value]
) {

  override def toString() = {
    value + " " + implies.mkString("{", ", ", "}") + ", " + excludes.mkString("{", ", ", "}")
  }
  
}