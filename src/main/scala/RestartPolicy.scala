package tugboat

trait RestartPolicy {
  def name: String = getClass.getSimpleName.stripSuffix("$")
}

object RestartPolicy {
  object Always extends RestartPolicy
  object OnFailure extends RestartPolicy
}
