class Star(circle: Int = 1, sector: Int = 1) extends SegmentObject(circle, sector) {
  override def draw: String = " ** "
}
