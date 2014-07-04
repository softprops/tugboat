package tugboat

sealed trait NetworkMode {
  def value = getClass.getSimpleName.toLowerCase.replace("""$""", "")
}

object NetworkMode {
  private [this] val ContainerSpec = """container:(.+)""".r
  case object Bridge extends NetworkMode
  case object None extends NetworkMode
  case class Container(id: String) extends NetworkMode {
    override def value = s"${super.value}:$id"
  }
  case object Host extends NetworkMode
  def unapply(str: String) = str match {
    case "bridge" => Some(Bridge)
    case "none"   => Some(NetworkMode.None)
    case "host"   => Some(Host)
    case ContainerSpec(id) =>
      Some(Container(id))
    case _ => scala.None
  }
}
