package planck

case class Vecd(entries: Seq[Double]) {
  def size = entries.size
  def apply(x: Int) = entries(x)
  lazy val lengthSq = entries.fold(0.0){_ + Math.pow(_, 2)}
  lazy val length = Math.sqrt(lengthSq)

  def ~=(that: Any) = that match {
    case Vecd(e) if e.size == size =>
      e.zip(entries).forall{case(x, y) => x-y < 0.01}
    case _ => false
  }

  def c(other: Vecd) =
    if(size != other.size)
      throw new SizeMismatchException

  def +(other: Vecd) = {
    c(other)
    Vecd(Seq.tabulate(size){x => this(x) + other(x)})
  }
  def -(other: Vecd) = {
    c(other)
    Vecd(Seq.tabulate(size){x => this(x) - other(x)})
  }
  def dot(other: Vecd) = {
    c(other)
    Seq.tabulate(size){x => this(x) * other(x)}.fold(0.0){_+_}
  }

  private def innerCross(other: Vecd) = Vecd(Seq(
    this(1) * other(2) - this(2) * other(1),
    this(2) * other(0) - this(0) * other(2),
    this(0) * other(1) - this(1) * other(0)
  ))

  def cross(other: Vecd) = {
    c(other)
    if(size != 3)
      throw new SizeMismatchException
    innerCross(other)
  }

  def normalize = {
    if(lengthSq == 0.0)
      Vecd(Seq.fill(size){0.0})
    else
      Vecd(Seq.tabulate(size){this(_) / length})
  }
}

object Vecd {
  def fill(x: Int, v: Double = 0.0) = Vecd(Seq.fill(x){v})
  def n(e: Double*) = Vecd(e)
}
