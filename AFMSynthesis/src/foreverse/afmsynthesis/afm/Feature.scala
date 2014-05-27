package foreverse.afmsynthesis.afm

class Feature(initName : String) extends VariationPoint(initName) {
  
  override def toString() : String = {
	"Feature(" + name + ")"
  }
  
}