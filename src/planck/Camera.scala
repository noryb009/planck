package planck

case class Camera(position: Vecd, target: Vecd, up: Vecd = Vecd.n(0, 1, 0)) {
  if(up.lengthSq != 1.0)
    throw new VectorNotNormalizedException
  lazy val lookDirOpp = (position - target).normalize
  lazy val right = up cross lookDirOpp
  lazy val lookMatrix = {
    // based on https://msdn.microsoft.com/bb281711
    val z = lookDirOpp
    val y = up
    val x = (y cross z).normalize
    val cols = (0 until 3).map{i => Vecd.n(x(i), y(i), z(i), 0)}
    val lastCol = Vecd(Seq(x, y, z).map{v => -(v dot position)} :+ 1.0)

    Matrixd(cols :+ lastCol)
  }

}

case class Mesh(name: String = "", vertices: Seq[Vecd])
case class YPR(yaw: Double, pitch: Double, roll: Double)
case class Entity(mesh: Mesh, translationVec: Option[Vecd] = None, ypr: Option[YPR] = None) {
  val translationMat = translationVec match {
    case Some(v) if v.size == 3 => Matrixd.translation(v(0), v(1), v(2))
    case Some(_) => throw new SizeMismatchException
    case None => Matrixd.id(4)
  }

  val rotationMat = ypr match {
    case Some(YPR(y, p, r)) => Quaterniond.fromYPR(y, p, r).toRotationMatrix
    case None => Matrixd.id(4)
  }
}
