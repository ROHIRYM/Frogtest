class SegmentObject(val circle: Int = 1, val sector: Int = 1) {
  override def equals(obj: Any): Boolean = {
    obj match {
      case segmentObject: SegmentObject =>
        segmentObject.circle == this.circle && segmentObject.sector == this.sector
      case _ => 
        false
    }
  }

  override def toString: String =
    s"(Circle $circle; Sector $sector)"
    
  def draw: String = " -- "
  
  def getDistance(obj: SegmentObject, maxSectors: Int): Double = {
    Math.sqrt(
      (this.circle - obj.circle)*(this.circle - obj.circle) +
        (if (this.sector > obj.sector) (maxSectors - this.sector + obj.sector)*(maxSectors - this.sector + obj.sector) else (this.sector - obj.sector)*(this.sector - obj.sector))
    )
  }
}
