class Cave(name: String, var connections: List[Cave]) {

  def isSmall = name.head.isLower

  def addConnections(cave: Cave) = connections = connections.appended(cave)

  def findPaths(route: List[String]): Int = {
    if (name == "end") return 1
    if (isSmall && route.contains(name)) return 0
    connections.map(_.findPaths(route.appended(name))).sum
  }

  def findPathsB(route: List[String], usedSmall: Boolean): Int = {
    if (name == "end") return 1
    if (isSmall && route.contains(name) && (name == "start" || usedSmall)) return 0
    connections.map(_.findPathsB(route.appended(name), isSmall && route.contains(name) || usedSmall)).sum
  }
}
