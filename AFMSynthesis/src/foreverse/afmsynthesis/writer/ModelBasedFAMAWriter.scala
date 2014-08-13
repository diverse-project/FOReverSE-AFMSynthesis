package foreverse.afmsynthesis.writer

import java.io.File
import foreverse.afmsynthesis.afm.AttributedFeatureModel
import es.us.isa.FAMA.models.FAMAAttributedfeatureModel.fileformats.AttributedWriter
import es.us.isa.FAMA.models.FAMAAttributedfeatureModel.FAMAAttributedFeatureModel
import es.us.isa.FAMA.models.FAMAAttributedfeatureModel.AttributedFeature
import foreverse.afmsynthesis.afm.Feature
import scala.collection.JavaConversions._
import es.us.isa.FAMA.models.FAMAAttributedfeatureModel.Relation
import es.us.isa.FAMA.models.featureModel.Cardinality
import es.us.isa.FAMA.Exceptions.FAMAException

class ModelBasedFAMAWriter extends FAMAWriter {

  override def write(afm : AttributedFeatureModel, file : File) {
	  val famaAFM = new FAMAAttributedFeatureModel
	  
	  writeHierarchy(afm, famaAFM)
	  println(famaAFM.getAttributedFeatures())
	 
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
    val famaRoot = new AttributedFeature
    famaRoot.setName(root.name)
    famaAFM.setRoot(famaRoot)
    
    def writeHierarchyRec(feature : Feature, famaFeature : AttributedFeature) {
    	
    	for (child <- hierarchy.children(feature)) {
    	  val famaChild = new AttributedFeature
    	  famaChild.setName(child.name)
    	  
    	
    	  val relation = new Relation
    	  relation.addCardinality(new Cardinality(1,1))
    	  relation.addDestination(famaChild)
    	  famaFeature.addRelation(relation)
    		
    	  writeHierarchyRec(child, famaChild)
    	}
    	
    }
    
    writeHierarchyRec(root, famaRoot)
  }
  
}