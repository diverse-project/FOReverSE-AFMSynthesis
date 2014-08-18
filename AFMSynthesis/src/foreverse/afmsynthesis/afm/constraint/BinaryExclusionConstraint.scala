package foreverse.afmsynthesis.afm.constraint

import foreverse.afmsynthesis.afm.Feature

case class BinaryExclusionConstraint(
    val feature : Feature, 
    val excluded : Feature)
    extends Constraint {

}