package foreverse.afmsynthesis.afm

class Attribute(initName : String, val domain : Domain) extends VariationPoint(initName) {
  
  override def toString() : String = {
	"Attribute(" + name + "," + domain + ")"
  }
  
} 