class Frog(circle: Int = 1, sector: Int = 1) extends SegmentObject(circle, sector) {
  def moveForward(): Frog = new Frog(circle, sector + 3)

  def moveForward2Left1(): Frog = new Frog(circle + 1, sector + 2)

  def moveForward2Right1(): Frog = new Frog(circle - 1, sector + 2)

  def moveForward1Left2(): Frog = new Frog(circle + 2, sector + 1)

  def moveForward1Right2(): Frog = new Frog(circle - 2, sector + 1)

  override def draw: String = " FF "
}
