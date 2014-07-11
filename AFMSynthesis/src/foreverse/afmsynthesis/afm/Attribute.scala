package foreverse.afmsynthesis.afm

class Attribute(
    val name : String,
    var domain : Domain
) {
  
  override def toString() : String = {
	"Attribute(" + name + ", " + domain + ")"
  }
  
} 