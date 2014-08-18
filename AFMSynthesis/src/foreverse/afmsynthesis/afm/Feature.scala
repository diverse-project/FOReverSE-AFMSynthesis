package foreverse.afmsynthesis.afm

import foreverse.afmsynthesis.afm.constraint.Variable

class Feature(
    val name : String,
    var attributes : List[Attribute],
    val parentRelation : Option[Relation],
    val childRelations : List[Relation] 
    
) extends Variable {
  
  def this(initName : String) = this(initName, Nil, None, Nil)
  
  override def toString() : String = {
	"Feature(" + name + ", " + attributes + ")"
  }
  
}