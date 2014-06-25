package tugboat

case class Image(
  id: String, created: Long, size: Long, virtualSize: Long,
  repoTags: List[String] = Nil, parentId: Option[String] = None)

case class SearchResult(
  name: String, description: String, trusted: Boolean, official: Boolean, stars: Int)
