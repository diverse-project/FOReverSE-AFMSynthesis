package foreverse.afmsynthesis.afm.constraint

import foreverse.afmsynthesis.afm.Attribute
import foreverse.afmsynthesis.afm.Feature

abstract class Value

case class FeatureValue(feature : Feature, positive : Boolean) extends Value

case class AttributeValue(attribute : Attribute, value : String) extends Value