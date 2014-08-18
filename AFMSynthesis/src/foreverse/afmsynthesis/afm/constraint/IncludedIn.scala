package foreverse.afmsynthesis.afm.constraint

import foreverse.afmsynthesis.afm.Attribute

case class IncludedIn(attribute : Attribute, values : Set[String]) extends Constraint {

}