package planck

/** A matrix made of column vectors.
  */
case class Matrixd(entries: Seq[Vecd]) {
  val sizeX = entries.size
  val sizeY = if(entries.isEmpty) 0 else entries.head.size
  def apply(i: Int): Vecd = entries(i)
  def apply(x: Int, y: Int): Double = entries(x)(y)

  private def singleVecMult(vec: Vecd) = {
    Seq.tabulate(sizeY){i =>
      Seq.tabulate(sizeX){k => entries(k)(i) * vec(k)}.fold(0.0){_+_}
    }
  }

  def *(m: Matrixd) = {
    if(sizeX != m.sizeY)
      throw new SizeMismatchException

    val colSeqs = Seq.tabulate(m.sizeX){j => singleVecMult(m(j))}
    Matrixd(colSeqs.map{Vecd(_)})
  }

  def *(vec: Vecd) = {
    if(sizeX != vec.size)
      throw new SizeMismatchException

    Vecd(singleVecMult(vec))
  }
}

object Matrixd {
  def fill(x: Int, y: Int, v: Double = 0.0) = Matrixd(Seq.fill(x){Vecd.fill(y, v)})
  def id(n: Int) = Matrixd(Seq.tabulate(n){x => Vecd(Seq.tabulate(n){y => if(x == y) 1 else 0})})
  def n(e: Vecd*) = Matrixd(e)
}
