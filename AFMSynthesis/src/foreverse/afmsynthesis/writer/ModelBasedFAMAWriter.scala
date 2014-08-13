package foreverse.afmsynthesis.writer

import java.io.File
import foreverse.afmsynthesis.afm.AttributedFeatureModel
import es.us.isa.FAMA.models.FAMAAttributedfeatureModel.fileformats.AttributedWriter
import es.us.isa.FAMA.models.FAMAAttributedfeatureModel.FAMAAttributedFeatureModel
import es.us.isa.FAMA.models.FAMAAttributedfeatureModel.AttributedFeature
import foreverse.afmsynthesis.afm.Feature
import scala.collection.JavaConversions._
import es.us.isa.FAMA.models.featureModel.Cardinality
import es.us.isa.FAMA.Exceptions.FAMAException
import foreverse.afmsynthesis.afm.Mandatory
import gsd.graph.ImplicationGraph
import foreverse.afmsynthesis.afm.Relation
import foreverse.afmsynthesis.afm.Mandatory
import foreverse.afmsynthesis.afm.Optional
import foreverse.afmsynthesis.afm.XorGroup
import foreverse.afmsynthesis.afm.OrGroup
import foreverse.afmsynthesis.afm.MutexGroup

class ModelBasedFAMAWriter extends FAMAWriter {

  override def write(afm : AttributedFeatureModel, file : File) {
	  val famaAFM = new FAMAAttributedFeatureModel

	  writeHierarchy(afm, famaAFM)
	 
	  val writer = new AttributedWriter
	  file.delete()
	  writer.writeFile(file.getAbsolutePath(), famaAFM)
	  println("out")
  }
  
  private def writeHierarchy(afm : AttributedFeatureModel, famaAFM : FAMAAttributedFeatureModel) {
    val afd = afm.diagram
    val hierarchy = afd.hierarchy
    val relations = afd.mandatoryRelations ::: 
				afd.mutexGroups ::: 
				afd.orGroups ::: 
				afd.xorGroups
    val roots = hierarchy.roots()
    assert(roots.size() == 1, "an AFM must have exactly one root")
    
    val root = roots.head
    println(afd.features)
    // Create FAMA features
    val features = afd.features.map(f => (f -> new AttributedFeature)).toMap
    for ((feature, famaFeature) <- features) {
      famaFeature.setName(feature.name)
    }
     
    // Set root
    val famaRoot = features(root)
    famaAFM.setRoot(famaRoot)
    
    // Set name and relations
    for (relation <- relations) {
      val famaRelation = new es.us.isa.FAMA.models.FAMAAttributedfeatureModel.Relation
      features(relation.parent).addRelation(famaRelation)
      relation.children.foreach(c => famaRelation.addDestination(features(c)))
      
      val (cardInf, cardSup) = relation match {
        case Optional(_,_) => (0,0)
        case Mandatory(_,_) => (1, 1)
        case MutexGroup(_,_) => (0, 1)
        case OrGroup(_,_) => (1, relation.children.size)
        case XorGroup(_,_) => (1, 1)
      }
      famaRelation.addCardinality(new Cardinality(cardInf, cardSup))
    }
    
    for ((feature, famaFeature) <- features) {
      if ((feature != root) && (!relations.exists(_.children.contains(feature)))) {
        val parent = hierarchy.parents(feature).head
        val famaParent = features(parent)
        
        val famaRelation = new es.us.isa.FAMA.models.FAMAAttributedfeatureModel.Relation
        famaRelation.addDestination(famaFeature)
        famaParent.addRelation(famaRelation)
        famaRelation.addCardinality(new Cardinality(0, 1))
        
      }
    }
    
    
  }

  
  
}