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

  def findPath(star: Star, frog: Frog): String = {
    assert(!isObjectOutOfBounds(star), s"Star $star is out of bounds")
    assert(!isObjectOutOfBounds(frog), s"Frog $frog is out of bounds")
    assert(!trees.contains(star), s"Star not allowed here $star")
    assert(!trees.contains(frog), s"Frog not allowed here $frog")
    
    this.star = star
    this.frog = frog
    
    val path = findPath(frog, List())
    var res = ""
    path.foreach(f => res = res + f + " -> ")
    res = res.substring(0, res.length - 4)
    
    res
  }

  def findPath(frog: Frog, accumulator: List[Frog]): List[Frog] = {
    val newList = frog :: accumulator
    newList
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
    obj.circle > circles || obj.circle < 1 || obj.sector > sectors || obj.sector < 1

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
