package foreverse.afmsynthesis.afm

class Feature(
    val name : String,
    var attributes : List[Attribute],
    val parentRelation : Option[Relation],
    val childRelations : List[Relation] 
    
) {
  
  def this(initName : String) = this(initName, Nil, None, Nil)
  
  override def toString() : String = {
	"Feature(" + name + ")"
  }
  
}