class Frog(circle: Int = 1, sector: Int = 1, val parentState: Frog) extends SegmentObject(circle, sector) {
  def moveForward(): Frog = new Frog(circle, sector + 3, this)

  def moveForward2Left1(): Frog = new Frog(circle + 1, sector + 2, this)

  def moveForward2Right1(): Frog = new Frog(circle - 1, sector + 2, this)

  def moveForward1Left2(): Frog = new Frog(circle + 2, sector + 1, this)

  def moveForward1Right2(): Frog = new Frog(circle - 2, sector + 1, this)

  override def draw: String = " FF "
}
