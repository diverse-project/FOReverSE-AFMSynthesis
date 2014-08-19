package foreverse.afmsynthesis.afm.constraint

import foreverse.afmsynthesis.afm.Attribute
import foreverse.afmsynthesis.afm.Feature

abstract class Variable extends Constraint

case class FeatureValue(feature : Feature, positive : Boolean) extends Variable

case class AttributeValue(attribute : Attribute, value : String) extends Variable