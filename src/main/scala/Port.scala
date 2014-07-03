package tugboat

sealed trait Port {
  def value: Int
  def kind: String
  def spec = s"$value/$kind"
}

object Port {
  private[this] val Spec = """(\d+)/(.+)""".r
  abstract class Kind(val kind: String) extends Port
  case class Tcp(value: Int) extends Kind("tcp")
  case class Udp(value: Int) extends Kind("udp")
  def unapply(spec: String): Option[Port] = spec match {
    case Spec(value, "tcp") =>
      Some(Tcp(value.toInt))
    case Spec(value, "udp") =>
      Some(Udp(value.toInt))
    case _ => None
  }
}
