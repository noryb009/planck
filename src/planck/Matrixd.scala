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
      Seq.tabulate(sizeX){k => entries(k)(i) * vec(k) }.fold(0.0){_+_}
    }
  }

  def *(m: Matrixd) = {
    if(sizeX != m.sizeY)
      throw new SizeMismatchException

    val cols = m.entries.map(singleVecMult)
    Matrixd(cols.map{Vecd(_)})
  }

  def *(vec: Vecd) = {
    if(sizeX != vec.size)
      throw new SizeMismatchException

    Vecd(singleVecMult(vec))
  }

  /** Apply self, a transformation matrix, to vec.
    *
    * @param vec The vector to apply the transformation to. This should be a
    *            3d vector.
    */
  def transform(vec: Vecd) = {
    if(sizeX != 4 || sizeY != 4 || vec.size != 3)
      throw new SizeMismatchException

    val vec4d = Vecd(vec.entries :+ 1.0)
    val transformed4d = this * vec4d
    Vecd.n(
      transformed4d(0) / transformed4d(3),
      transformed4d(1) / transformed4d(3),
      transformed4d(2) / transformed4d(3)
    )
  }
}

object Matrixd {
  def fill(x: Int, y: Int, v: Double = 0.0) = Matrixd(Seq.fill(x){Vecd.fill(y, v)})
  def id(n: Int) = Matrixd(Seq.tabulate(n){x => Vecd(Seq.tabulate(n){y => if(x == y) 1 else 0})})
  def n(e: Vecd*) = Matrixd(e)

  def perspectiveFOV(fov: Double, aspectRatio: Double, near: Double, far: Double) = {
    // based on https://msdn.microsoft.com/bb281728
    val h = 1.0 / Math.tan(fov / 2)
    val w = h / aspectRatio
    val nearMinusFarInverse = 1 / (near - far)
    val farDist = (far + near) * nearMinusFarInverse
    val farDist2 = 2 * far * near * nearMinusFarInverse

    val c1 = Vecd.n(w, 0, 0, 0)
    val c2 = Vecd.n(0, h, 0, 0)
    val c3 = Vecd.n(0, 0, farDist, -1)
    val c4 = Vecd.n(0, 0, farDist2, 0)

    Matrixd.n(c1, c2, c3, c4)
  }
}
