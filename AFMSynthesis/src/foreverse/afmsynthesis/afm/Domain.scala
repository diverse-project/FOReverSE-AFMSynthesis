package foreverse.afmsynthesis.afm

abstract class Domain(val _nullValue : Any) {
	def isNullValue(value : Any) : Boolean = {
		value == _nullValue
	}
}

