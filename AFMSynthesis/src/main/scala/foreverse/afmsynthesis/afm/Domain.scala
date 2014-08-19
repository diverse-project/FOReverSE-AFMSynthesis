package foreverse.afmsynthesis.afm

class Domain(
    val values : Set[String], 
    val nullValue : String, 
    val lessThan : (String, String) => Boolean
) {
  
	def isNullValue(value : String) : Boolean = {
		value == nullValue
	}
	
	override def toString() : String = {
		"Domain(" + values.mkString("{", ", ", "}") + ", " + nullValue + ")"
	}
	
}

