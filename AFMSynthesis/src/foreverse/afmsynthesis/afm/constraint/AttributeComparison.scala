package foreverse.afmsynthesis.afm.constraint

import foreverse.afmsynthesis.afm.Attribute

case class AttributeComparison(attribute : Attribute, operator : Operator, value : String) extends Value {

}