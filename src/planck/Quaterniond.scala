package planck

/** Based on
  * http://graphics.cs.williams.edu/courses/cs371/s07/reading/quaternions.pdf
  *
  * @param entriesSeq A sequence of entries, (w,x,y,z)
  */
case class Quaterniond(entriesSeq: Seq[Double]) {
  if(entriesSeq.size != 4)
    throw new SizeMismatchException

  val entries = Vecd(entriesSeq)

  def apply(i: Integer) = entries(i)

  def normalize = Quaterniond.v(entries.normalize)
  val lengthSq = entries.lengthSq
  lazy val length = entries.length

  def ~=(that: Any) = that match {
    case e: Quaterniond => this.entries ~= e.entries
    case _ => false
  }

  def *(o: Quaterniond) = {
    val q = this.entries.entries
    val r = o.entries.entries

    val qw = q(0)
    val rw = r(0)
    val qx = q(1)
    val rx = r(1)
    val qy = q(2)
    val ry = r(2)
    val qz = q(3)
    val rz = r(3)

    val w = qw*rw - qx*rx - qy*ry - qz*rz
    val x = qw*rx + qx*rw + qy*rz - qz*ry
    val y = qw*ry - qx*rz + qy*rw + qz*rx
    val z = qw*rz + qx*ry - qy*rx + qz*rw

    Quaterniond(Seq(w, x, y, z))
  }

  def toRotationMatrix = {
    val q = this.entries.entries

    val qw = q(0)
    val qx = q(1)
    val qy = q(2)
    val qz = q(3)

    val qx2 = 2*qx*qx
    val qy2 = 2*qy*qy
    val qz2 = 2*qz*qz

    val qwqx = 2*qw*qx
    val qwqy = 2*qw*qy
    val qwqz = 2*qw*qz
    val qxqy = 2*qx*qy
    val qxqz = 2*qx*qz
    val qyqz = 2*qy*qz

    val col1 = Vecd.n(1-qy2-qz2, qxqy+qwqz, qxqz-qwqy, 0)
    val col2 = Vecd.n(qxqy-qwqz, 1-qx2-qz2, qyqz+qwqx, 0)
    val col3 = Vecd.n(qxqz+qwqy, qyqz-qwqx, 1-qx2-qy2, 0)
    val col4 = Vecd.n(0, 0, 0, 1)

    Matrixd.n(col1, col2, col3, col4)
  }
}

object Quaterniond {
  def n(w: Double, x: Double, y: Double, z: Double) = Quaterniond(Seq(w,x,y,z))
  def v(vec: Vecd) = Quaterniond(vec.entries)

  /** Create a quaternion from yaw, pitch and roll.
    *
    * @param yaw Rotation in radians around the X axis.
    * @param pitch Rotation in radians around the Y axis.
    * @param roll Rotation in radians around the Z axis.
    * @return The rotation quaternion.
    */
  def fromYPR(yaw: Double, pitch: Double, roll: Double) = {
    val c1 = Math.cos(yaw / 2)
    val c2 = Math.cos(pitch / 2)
    val c3 = Math.cos(roll / 2)
    val s1 = Math.sin(yaw / 2)
    val s2 = Math.sin(pitch / 2)
    val s3 = Math.sin(roll / 2)

    val w = c1*c2*c3 - s1*s2*s3
    val x = s1*s2*c3 + c1*c2*s3
    val y = s1*c2*c3 + c1*s2*s3
    val z = c1*s2*c3 - s1*c2*s3

    Quaterniond(Seq(w, x, y, z))
  }
}
