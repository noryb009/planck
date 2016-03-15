package planck

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

    Vecd.n(x, y) // TODO: Should x and y be converted to integers?
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

    // TODO: Make tail recursive, if possible.
    def breakMidpoints(v1: Vecd, v2: Vecd): Seq[Vecd] = {
      val diff = v1 - v2
      if(diff.lengthSq < 4) // diff.length < 2
        Seq()
      else {
        val mid = v2 + (diff * 0.5)
        mid +: (breakMidpoints(v1, mid) ++ breakMidpoints(mid, v2))
      }
    }

    breakMidpoints(v1, v2)
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

      val points = m.vertices.map(projectPoint)

      val pointsShifted = points.tail :+ points.head

      val linePoints = points.zip(pointsShifted).flatMap{case(a, b) => drawLine(a, b)}

      points ++ linePoints
    }

    allPoints.map{v => (v(0).toInt, v(1).toInt)}.toSet
  }
}
