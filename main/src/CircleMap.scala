class CircleMap {
  private var circles: Int = 1
  private var sectors: Int = 1

  private var trees: List[Tree] = List()
  private var star: Star = null
  private var frog: Frog = null

  def this(circles: Int = 1, sectors: Int = 1) = {
    this()

    assert(circles >= 1 && sectors >= 1, "Wrong circle map")

    this.circles = circles
    this.sectors = sectors
  }

  def findPath(): String = {
    val path = findPath(frog, List())
    var res = ""

    if (path == null || path.isEmpty) {
      res = "No path"
    } else {
      path.foreach(f => res = res + f + " -> ")
      res = res.substring(0, res.length - 4)
    }

    res
  }

  def findPath(frog: Frog, accumulator: List[Frog]): List[Frog] = {
    val frogTmp = if (isObjectOutOfSectors(frog)) new Frog(frog.circle, frog.sector - sectors) else frog

    if (isObjectOutOfCircles(frogTmp)) {
      return null
    }
    if (accumulator.contains(frogTmp)) {
      return null
    }
    if (trees.contains(frogTmp)) {
      return null
    }

    val newList = frogTmp :: accumulator

    if (frogTmp == star) {
      return newList
    }

    val results = List(
      findPath(frogTmp.moveForward(), newList),
      findPath(frogTmp.moveForward2Left1(), newList),
      findPath(frogTmp.moveForward2Right1(), newList),
      findPath(frogTmp.moveForward1Left2(), newList),
      findPath(frogTmp.moveForward1Right2(), newList)
    ).filter(list => list != null)

    if (results.isEmpty) {
      return null
    }

    results.minBy(_.length)
  }

  def addStar(star: Star): Unit = {
    assert(!isObjectOutOfBounds(star), s"Star $star is out of bounds")
    assert(!trees.contains(star), s"Star not allowed here $star")

    this.star = star
  }

  def addFrog(frog: Frog): Unit = {
    assert(!isObjectOutOfBounds(frog), s"Frog $frog is out of bounds")
    assert(!trees.contains(frog), s"Frog not allowed here $frog")

    this.frog = frog
  }

  def addTrees(trees: List[Tree]): Unit = {
    trees.foreach(t => addTree(t))
  }

  def addTree(tree: Tree): Unit = {
    assert(!isObjectOutOfBounds(tree), s"Tree $tree is out of bounds")
    assert(!trees.contains(tree), s"Duplicate tree $tree")
    trees = tree :: this.trees
  }

  private def isObjectOutOfBounds(obj: SegmentObject): Boolean =
    isObjectOutOfCircles(obj) || isObjectOutOfSectors(obj)

  private def isObjectOutOfCircles(obj: SegmentObject): Boolean =
    obj.circle > circles || obj.circle < 1

  private def isObjectOutOfSectors(obj: SegmentObject): Boolean =
    obj.sector > sectors || obj.sector < 1

  private def drawSegment(obj: SegmentObject): String = {
    if (trees.contains(obj)) {
      return trees.head.draw
    } else if (star == obj) {
      return star.draw
    } else if (frog == obj) {
      return frog.draw
    }
    obj.draw
  }

  override def toString: String =
    "\n" + sectors.until(0).by(-1).map(sn => f" $sn%02d ").reduce((s1,s2) => s1+s2) + "\n" +
      0.until(circles).map(
        c => sectors.until(0).by(-1).map(
          s => drawSegment(new SegmentObject(c+1,s))
        ).reduce((s1,s2) => s1+s2) + "  %d \n".format(c+1)
      ).reduce((c1,c2) => c1+c2)
}
