import java.awt.{Graphics2D, Graphics, Dimension, Color}
import java.awt.image.BufferedImage
import javax.swing.{JPanel, JFrame}

import planck._

object Main extends App {
  def v(e: Double*) = Vecd(e)
  val cube = Mesh("Cube", Seq(
    v(-1, -1, -1),
    v(-1, -1,  1),
    v(-1,  1, -1),
    v(-1,  1,  1),
    v( 1, -1, -1),
    v( 1, -1,  1),
    v( 1,  1, -1),
    v( 1,  1,  1)
  ))

  class Panel extends JPanel {
    val canvas: BufferedImage = new BufferedImage(Renderer.w, Renderer.h, BufferedImage.TYPE_INT_RGB)

    def render(c: Camera, e: Seq[Entity]) = {
      val points = Renderer.render(c, e)
      val white = new Color(255, 255, 255)
      val black = new Color(0, 0, 0)
      (0 until Renderer.w).foreach{x =>
        (0 until Renderer.h).foreach{y =>
          val c = if(points(x, y))
            white
          else
            black
          canvas.setRGB(x, y, c.getRGB)
        }
      }

      repaint()
    }

    override def getPreferredSize = new Dimension(canvas.getWidth, canvas.getHeight)
    override def paintComponent(g: Graphics) = {
      super.paintComponent(g)
      val g2 = g.asInstanceOf[Graphics2D]
      g2.drawImage(canvas, null, null)
    }
  }

  var frame = new JFrame("")
  var panel = new Panel
  frame.add(panel)
  frame.pack()
  frame.setVisible(true)
  frame.setResizable(false)
  frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)

  val camera = Camera(Vecd.n(10, 0, 0), Vecd.n(0, 0, 0))

  var n = 0.0
  var m = 0.0
  while(true) {
    val t = Vecd.n(0,n,0)
    val r = YPR(0,m,n)
    n += 0.01
    m += 0.04
    panel.render(camera, Seq(Entity(cube, Some(t), Some(r))))
    Thread.sleep(16)
  }
}
