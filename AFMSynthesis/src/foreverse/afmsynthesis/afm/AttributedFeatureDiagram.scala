package foreverse.afmsynthesis.afm

import gsd.graph.ImplicationGraph
import foreverse.afmsynthesis.afm.constraint.CrossTreeConstraint

class AttributedFeatureDiagram ( 
  val features : List[Feature],
  val hierarchy : ImplicationGraph[Feature],
  val mandatoryRelations : List[Mandatory],
  val mutexGroups : List[MutexGroup],
  val orGroups : List[OrGroup],
  val xorGroups : List[XorGroup],
  val constraints : List[CrossTreeConstraint]
) {
  
}