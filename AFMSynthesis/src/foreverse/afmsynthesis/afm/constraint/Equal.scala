package foreverse.afmsynthesis.afm.constraint

import foreverse.afmsynthesis.afm.Attribute

case class Equal(initAttribute : Attribute, initValue : String) 
extends AttributeOperator(initAttribute, initValue) {

}