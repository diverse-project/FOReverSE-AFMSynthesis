package foreverse.afmsynthesis.afm

case class BinaryImplicationConstraint(
    val feature : Feature, 
    val implied : Feature)
    extends CrossTreeConstraint {

}