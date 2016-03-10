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
    (x.toInt, y.toInt)
  }

  def render(meshes: Seq[Mesh]) = {
    val camera = Camera(Vecd.n(10, 0, 0), Vecd.n(0, 0, 0))

    val viewMatrix = camera.lookMatrix
    val projectionMatrix = Matrixd.perspectiveFOV(0.78, w / h.toDouble, 0.01, 100)
    val vpMatrix = projectionMatrix * viewMatrix

    val allPoints = meshes.flatMap{m =>
      lazy val yprMatrix: Matrixd = ???
      lazy val translateMatrix: Matrixd = ???
      //val modelMatrix = yprMatrix * tranMatrix
      val modelMatrix = Matrixd.id(4)

      val mvpMatrix = modelMatrix * vpMatrix

      def projectPoint(v: Vecd) = {
        val tPoint = mvpMatrix.transform(v)
        pointToScreen(tPoint)
      }

      val points = m.vertices.map(projectPoint)

      points
    }

    allPoints.toSet
  }
}