package foreverse.afmsynthesis.afm

abstract class Value

case class FeatureValue(feature : Feature, positive : Boolean) extends Value

case class AttributeValue(attribute : Attribute, value : String) extends Value