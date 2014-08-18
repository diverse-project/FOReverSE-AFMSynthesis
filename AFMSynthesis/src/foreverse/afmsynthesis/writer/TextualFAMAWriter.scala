package foreverse.afmsynthesis.writer

import java.io.FileWriter
import foreverse.afmsynthesis.afm.OrGroup
import foreverse.afmsynthesis.afm.MutexGroup
import foreverse.afmsynthesis.afm.Mandatory
import foreverse.afmsynthesis.afm.AttributedFeatureModel
import foreverse.afmsynthesis.afm.Relation
import foreverse.afmsynthesis.afm.XorGroup
import java.io.File
import foreverse.afmsynthesis.afm.Feature
import foreverse.afmsynthesis.afm.constraint.BinaryImplicationConstraint
import foreverse.afmsynthesis.afm.constraint.BinaryExclusionConstraint
import scala.collection.JavaConversions._

class TextualFAMAWriter extends FAMAWriter {

  override def write(afm : AttributedFeatureModel, file : File) {
    val writer = new FileWriter(file)
    
    writer.write("%Relationships\n")
    writeHierarchy(afm, writer)
    
    if (afm.diagram.features.exists(!_.attributes.isEmpty)) {
    	writer.write("\n%Attributes\n")
    	writeAttributes(afm, writer)
    }
    
    if (afm.constraint.isDefined || !afm.diagram.constraints.isEmpty) {
    	writer.write("\n%Constraints\n")
    	writeConstraints(afm, writer)
    }
    
    
    writer.close();
  }
  
  private def writeHierarchy(afm : AttributedFeatureModel, writer : FileWriter) {
    val afd = afm.diagram
    val hierarchy = afd.hierarchy
    val relations = afd.mandatoryRelations ::: 
				afd.mutexGroups ::: 
				afd.orGroups ::: 
				afd.xorGroups
    val roots = hierarchy.roots()
    assert(roots.size() == 1, "an AFM must have exactly one root")
    
    
    
    def writeHierarchyRec(feature : Feature) {
    	val children = hierarchy.children(feature)
    	if (!children.isEmpty()) {
    		writer.write(feature.name)
	    	writer.write(" : ")
	    	
	    	writeChildren(children.toList, relations, writer)

	    	writer.write(";\n")
	    	
	    	for (child <- children) {
	    	  writeHierarchyRec(child)
	    	}
    	}
    	
    }
    
    writeHierarchyRec(roots.head)
  }
  
  private def writeChildren(children: List[Feature], relations : List[Relation], writer : FileWriter) {
    
	  def featuresToString(set : Set[Feature]) : String = {
	    set.map(_.name).mkString(" ")
	  }
	  
	  val processedChildren = collection.mutable.Set.empty[Feature]
	  
	  for (child <- children if !processedChildren.contains(child)) {
		  val relation = relations.find(_.children.contains(child))
		  val relationString = relation match {
		    case Some(Mandatory(_, _)) => 
		      processedChildren += child
		      child.name
		    case Some(MutexGroup(_, features)) => 
		      processedChildren ++= features 
		      "[0,1]{" + featuresToString(features) + "}"
		    case Some(OrGroup(_, features)) => 
		      processedChildren ++= features
		      "[1," + features.size + "]{" + featuresToString(features) + "}"
		    case Some(XorGroup(_, features)) => 
		      processedChildren ++= features
		      "[1,1]{" + featuresToString(features) + "}"
//		    case Some(Optional(_, _)) | None => "[" + child.name + "]" 
		    case _ =>
		      processedChildren += child
		      "[" + child.name + "]"
		  }
		  writer.write(relationString + " ")
	  }
	  
  }
  
  private def writeAttributes(afm : AttributedFeatureModel, writer : FileWriter) {
    val features = afm.diagram.features
    for (feature <- features; 
    attribute <- feature.attributes) {
      writer.write(feature.name + "." + attribute.name)
      val domain = attribute.domain
      val values = domain.values.toList.sortWith(domain.lessThan)
      val (min, max) = (values.head, values.last)
      val nullValue = if (domain.nullValue.isEmpty()) {
        "0"
      } else {
        domain.nullValue
      }
      writer.write(": Integer[" + min + " to " + max + "]," 
          + min + "," + nullValue + ";\n" )
    }
  }

  private def writeConstraints(afm : AttributedFeatureModel, writer : FileWriter) {
    for (constraint <- afm.diagram.constraints) {
      val constraintString = constraint match {
        case BinaryImplicationConstraint(feature, implied) => feature.name + " REQUIRES " + implied.name
        case BinaryExclusionConstraint(feature, excluded) => feature.name + " EXCLUDES " + excluded.name
        case _ => ""
      }
      
      writer.write(constraintString + ";\n")
    }
  }
  
}