package foreverse.afmsynthesis.afm.constraint

import foreverse.afmsynthesis.afm.Feature

case class BinaryImplicationConstraint(
    val feature : Feature, 
    val implied : Feature)
    extends CrossTreeConstraint {

}