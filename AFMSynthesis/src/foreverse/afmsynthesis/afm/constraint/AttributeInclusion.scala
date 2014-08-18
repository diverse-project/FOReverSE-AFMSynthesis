package foreverse.afmsynthesis.afm.constraint

import foreverse.afmsynthesis.afm.Attribute

case class AttributeInclusion(attribute : Attribute, values : Set[String]) extends Value {

}