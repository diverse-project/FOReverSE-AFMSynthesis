package foreverse.afmsynthesis.afm

class Feature(
    val name : String,
    val attributes : List[Attribute],
    val parentRelation : Option[Relation],
    val childRelations : List[Relation] 
    
) {
  
  override def toString() : String = {
	"Feature(" + name + ")"
  }
  
}