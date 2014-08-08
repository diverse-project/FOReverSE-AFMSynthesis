package foreverse.afmsynthesis.afm

case class BinaryExclusionConstraint(
    val feature : Feature, 
    val excluded : Feature)
    extends CrossTreeConstraint {

}