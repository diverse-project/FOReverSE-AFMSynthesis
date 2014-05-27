package foreverse.afmsynthesis.afm

abstract class Domain(val _nullValue : Any) {
	def isNullValue(value : Any) : Boolean = {
		value == _nullValue
	}
}
case class BooleanDomain extends Domain(false)
case class IntegerDomain(nullValue : Int) extends Domain(nullValue)
case class RealDomain(nullValue : Double) extends Domain
case class EnumDomain(val literals : Set[String], nullValue : String) extends Domain(nullValue)