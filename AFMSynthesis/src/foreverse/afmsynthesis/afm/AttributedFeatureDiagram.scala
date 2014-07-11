package foreverse.afmsynthesis.afm

class AttributedFeatureDiagram ( 
  val features : List[Feature],
  val root : Feature,
  val relations : List[Relation],
  val constraints : List[CrossTreeConstraint]
) {
  
}