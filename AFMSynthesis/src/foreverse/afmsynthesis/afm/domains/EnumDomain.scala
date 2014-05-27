package foreverse.afmsynthesis.afm.domains

import foreverse.afmsynthesis.afm.Domain

case class EnumDomain(val literals : Set[String], nullValue : String) extends Domain(nullValue) {

}