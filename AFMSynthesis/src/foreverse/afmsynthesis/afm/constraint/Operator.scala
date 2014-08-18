package foreverse.afmsynthesis.afm.constraint

abstract class Operator
case class Equal extends Operator
case class NotEqual extends Operator
case class Less extends Operator
case class Greater extends Operator
case class LessOrEqual extends Operator
case class GreaterOrEqual extends Operator
