class CircleMap {
  private var circles: Int = 1
  private var sectors: Int = 1

  private var trees: List[Tree] = List()
  private var star: Star = null
  private var frog: Frog = null
  private var pathElements: List[PathElement] = List()

  def this(circles: Int = 1, sectors: Int = 1) = {
    this()

    assert(circles >= 1 && sectors >= 1, "Wrong circle map")

    this.circles = circles
    this.sectors = sectors
  }

  def findPathForFrogToStar(): String = {
    findPath()

    this.toString()
  }

  def findPath(): List[SegmentObject] = {
    assert(frog != null, "We need the frog")
    assert(star != null, "We need the star")

    var path = findPath(List(frog), List()).parentState
    while (path != null && path.parentState != null) {
      pathElements = PathElement(path.circle, path.sector) :: pathElements
      path = path.parentState
    }

    frog :: pathElements.appended(star)
  }

  private def findPath(frontier: List[Frog], exploredSet: List[Frog]): Frog = {
    assert(frontier.nonEmpty, "No path")

    var head = frontier.head
    if (isObjectOutOfSectors(head)) {
      head = Frog(head.circle, head.sector - sectors, head.parentState)
    }

    if (head == star) {
      return head
    }

    if (exploredSet.contains(head) || trees.contains(head) || isObjectOutOfCircles(head)) {
      return findPath(frontier.tail, exploredSet)
    }

    val nextSteps = List(
      head.moveForward(),
      head.moveForward2Left1(),
      head.moveForward2Right1(),
      head.moveForward1Left2(),
      head.moveForward1Right2()
    )

    findPath(frontier.tail ::: nextSteps, head :: exploredSet)
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
    assert(frog != tree && star != tree, s"Tree not allowed here $tree")
    trees = tree :: trees
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
    } else if (pathElements.contains(obj)) {
      return pathElements.head.draw
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
