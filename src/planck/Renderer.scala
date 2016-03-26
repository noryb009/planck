package planck

import java.awt.Color

import scala.annotation.tailrec
import scala.util.Random

/** A 3D renderer, based on
  * https://blogs.msdn.microsoft.com/davrous/2013/06/13/tutorial-series-learning-how-to-write-a-3d-soft-engine-from-scratch-in-c-typescript-or-javascript/
  */
object Renderer {
  val w = 320
  val h = 200

  def mix(min: Double, max: Double, percent: Double) = min + (max - min) * percent

  def scanLine(y: Int, leftTop: Vecd, leftBot: Vecd, rightTop: Vecd, rightBot: Vecd) = {
    def percent(num: Double, denom: Double) = if(denom == 0.0) 1.0 else num / denom

    val yDouble = y.toDouble

    val percentYLeft = percent(yDouble - leftTop(1), leftBot(1) - leftTop(1))
    val percentYRight = percent(yDouble - rightTop(1), rightBot(1) - rightTop(1))

    val xLeft = mix(leftTop(0), leftBot(0), percentYLeft).toInt
    val xRight = mix(rightTop(0), rightBot(0), percentYRight).toInt
    val zLeft = mix(leftTop(2), leftBot(2), percentYLeft)
    val zRight = mix(rightTop(2), rightBot(2), percentYRight)

    (xLeft to xRight).map{x => Vecd.n(x, yDouble, mix(zLeft, zRight, percent(x - xLeft, xRight - xLeft)))}
  }

  def drawTriangle(x: Vecd, y: Vecd, z: Vecd) = {
    val Seq(a, b, c) = Seq(x, y, z).sortBy{t => (t(1), t(0))}
    val Seq(ax, ay, _) = a.entries
    val Seq(bx, by, _) = b.entries
    val Seq(cx, cy, _) = c.entries

    val abDiffX = bx - ax
    val acDiffX = cx - ax
    val abDiffY = by - ay
    val acDiffY = cy - ay

    val abInverseSlope = if(abDiffY == 0) 0 else abDiffX / abDiffY
    val acInverseSlope = if(acDiffY == 0) 0 else acDiffX / acDiffY

    if((abDiffY == 0 && a(0) > b(0)) || (abDiffY != 0 && abInverseSlope < acInverseSlope)) {
      // b on left of AC
      val top = (ay.toInt until by.toInt).flatMap{scanLine(_, a, b, a, c)}
      val bot = (by.toInt to cy.toInt).flatMap{scanLine(_, b, c, a, c)}
      top ++ bot
    } else {
      // b on right of AC
      val top = (ay.toInt until by.toInt).flatMap{scanLine(_, a, c, a, b)}
      val bot = (by.toInt to cy.toInt).flatMap{scanLine(_, a, c, b, c)}
      top ++ bot
    }
  }

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

    Vecd.n(x.toInt, y.toInt, v(2))
  }

  /** Creates a sequence of vectors representing points on a line.
    *
    * @param v1 The first vertex.
    * @param v2 The second vertex.
    * @return A sequence of points in the line.
    */
  def drawLine(v1: Vecd, v2: Vecd) = {
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
      /// TODO: z
      val newAcc = Vecd.n(x1, y1, 0) +: acc

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
      val rotationMatrix = e.rotationMat
      val translateMatrix = e.translationMat
      val modelMatrix = translateMatrix * rotationMatrix

      val mvpMatrix = vpMatrix * modelMatrix

      def projectPoint(v: Vecd) = {
        val tPoint = mvpMatrix.transform(v)
        pointToScreen(tPoint)
      }

      val points = m.vertices.map(projectPoint).toIndexedSeq

      val facePoints = m.faces.flatMap{f =>
        val seed = f.a + f.b * 3 + f.c * 7
        // TODO: pure
        val r = new Random(seed)

        val colour = new Color(r.nextInt(256), r.nextInt(256), r.nextInt(256))

        val lines =
          drawLine(points(f.a), points(f.b)) ++
          drawLine(points(f.a), points(f.c)) ++
          drawLine(points(f.b), points(f.c))

        val faces =
          drawTriangle(points(f.a), points(f.b), points(f.c))

        val pixels = faces //++ lines

        pixels.map{(_, colour)}
      }

      facePoints
    }

    type MapZColour = Map[(Double, Double), (Double, Color)]

    val allPointsZColour = allPoints.foldLeft[MapZColour](Map()){case(m, (v, c)) =>
      val key = (v(0), v(1))
      val z = v(2)
      m.get(key) match {
        case Some((z2, _)) if z >= z2 - 0.001 => m
        case _ => m + (key -> (z, c))
      }
    }

    allPointsZColour.mapValues{case(_, c) => c}.withDefaultValue(Color.BLACK)
  }
}
