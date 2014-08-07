package foreverse.afmsynthesis.algorithm

import foreverse.afmsynthesis.afm.AttributedFeatureModel
import java.io.File
import java.io.FileWriter
import foreverse.afmsynthesis.afm.Feature
import scala.collection.JavaConversions._

class FAMAWriter {

  def write(afm : AttributedFeatureModel, file : File) {
    val writer = new FileWriter(file)
    
    writer.write("%Relationships\n")
    writeHierarchy(afm, writer)
    
    writer.write("\n%Attributes\n")
    writeAttributes(afm, writer)
    
    writer.write("\n%Constraints\n")
    
    writer.close();
  }
  
  private def writeHierarchy(afm : AttributedFeatureModel, writer : FileWriter) {
    val hierarchy = afm.diagram.hierarchy
    val mandatoryRelations = afm.diagram.mandatoryRelations
    val roots = hierarchy.roots()
    assert(roots.size() == 1, "an AFM must have exactly one root")
    
    def writeHierarchyRec(feature : Feature) {
    	val children = hierarchy.children(feature)
    	if (!children.isEmpty()) {
    		writer.write(feature.name)
	    	writer.write(" : ")
	    	
	    	for (child <- children) {
	    	  val childString = if (mandatoryRelations.exists(_.child == child)) {
	    	      child.name
	    	    } else {
	    	      "[" + child.name + "]"
	    	    }
	    	  writer.write(childString + " ")
	    	}
	    	
	    	writer.write(";\n")
	    	
	    	for (child <- children) {
	    	  writeHierarchyRec(child)
	    	}
    	}
    	
    }
    
    writeHierarchyRec(roots.head)
  }
  
  private def writeAttributes(afm : AttributedFeatureModel, writer : FileWriter) {
    val features = afm.diagram.features
    for (feature <- features; 
    attribute <- feature.attributes) {
      writer.write(feature.name + "." + attribute.name)
      // TODO : write domain
    }
  }
  
}