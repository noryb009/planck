package planck

import scala.annotation.tailrec

/** A 3D renderer, based on
  * https://blogs.msdn.microsoft.com/davrous/2013/06/13/tutorial-series-learning-how-to-write-a-3d-soft-engine-from-scratch-in-c-typescript-or-javascript/
  */
object Renderer {
  val w = 320
  val h = 200

  /** Convert a transformed vertex in an OpenGL-style clipping area to a viewport.
    * The viewport's origin is at the top left of the screen.
    *
    * @param v The vertex.
    */
  def pointToScreen(v: Vecd) = {
    // If v.x is 0, it is in middle of the screen.
    // If v.x is 1, it is at right of the screen.
    val x = (v(0) + 1) * w / 2
    // If v.y is 0, it is in middle of the screen.
    // If v.y is 1, it is at top of the screen.
    val y = (-v(1) + 1) * h / 2

    Vecd.n(x.toInt, y.toInt)
  }

  /** Creates a sequence of vectors representing points on a line.
    *
    * @param v1 The first vertex.
    * @param v2 The second vertex.
    * @return A sequence of points in the line.
    */
  def drawLine(v1: Vecd, v2: Vecd) = {
    if(v1.size != 2 || v2.size != 2)
      throw new SizeMismatchException

    val x1 = v1(0).toInt
    val y1 = v1(1).toInt
    val x2 = v2(0).toInt
    val y2 = v2(1).toInt

    val dx = Math.abs(x1 - x2)
    val dy = Math.abs(y1 - y2)
    val sx = if(x1 < x2) 1 else -1
    val sy = if(y1 < y2) 1 else -1
    val err = dx - dy

    @tailrec
    def stepBresenham(x1: Int, y1: Int, err: Int, acc: List[Vecd] = List()): Seq[Vecd] = {
      val newAcc = Vecd.n(x1, y1) +: acc

      if(x1 == x2 && y1 == y2)
        acc
      else {
        val err2 = err * 2

        val nextX = err2 > -dy
        val nextY = err2 < dx
        val newX = if(nextX) x1 + sx else x1
        val newY = if(nextY) y1 + sy else y1
        val newErr = err + (if(nextY) dx else 0) - (if(nextX) dy else 0)

        stepBresenham(newX, newY, newErr, newAcc)
      }
    }

    stepBresenham(x1, y1, err)
  }

  def render(camera: Camera, entities: Seq[Entity]) = {
    val viewMatrix = camera.lookMatrix
    val projectionMatrix = Matrixd.perspectiveFOV(0.78, w / h.toDouble, 0.01, 100)
    val vpMatrix = projectionMatrix * viewMatrix

    val allPoints = entities.flatMap{e =>
      val m = e.mesh
      val rotationMatrix: Matrixd = e.rotationMat
      val translateMatrix: Matrixd = e.translationMat
      val modelMatrix = translateMatrix * rotationMatrix

      val mvpMatrix = vpMatrix * modelMatrix

      def projectPoint(v: Vecd) = {
        val tPoint = mvpMatrix.transform(v)
        pointToScreen(tPoint)
      }

      val points = m.vertices.map(projectPoint).toIndexedSeq

      val linePoints = m.faces.flatMap{f =>
        drawLine(points(f.a), points(f.b)) ++
        drawLine(points(f.a), points(f.c)) ++
        drawLine(points(f.b), points(f.c))
      }

      linePoints
    }

    allPoints.map{v => (v(0).toInt, v(1).toInt)}.toSet
  }
}
