package foreverse.afmsynthesis.afm

import foreverse.afmsynthesis.afm.constraint.Variable

class Attribute(
    val name : String,
    var domain : Domain
) extends Variable {
  
  override def toString() : String = {
	"Attribute(" + name + ", " + domain + ")"
  }
  
} 