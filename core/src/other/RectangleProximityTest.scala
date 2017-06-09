package other

import java.awt.event.{MouseAdapter, MouseEvent}
import java.awt.{Color, Dimension, Graphics}
import java.awt.image.BufferedImage
import javax.swing.{JFrame, JPanel}

import com.phoenixkahlo.hellcraft.util.{Rectangle, RectangleProxmimity, V2F, V3F}

/**
  * Created by kahlo on 6/9/2017.
  */
object RectangleProximityTest {

  def main(args: Array[String]): Unit = {
    val rect = Rectangle(V2F(100, 100), V2F(200, 200))
    val prox = RectangleProxmimity(rect, 50)

    var clicked: Option[V2F] = None

    val panel = new JPanel() {
      override def paintComponent(g: Graphics): Unit = {
        val buffer = new BufferedImage(300, 300, BufferedImage.TYPE_INT_RGB)
        for {
          x <- 0 until 300
          y <- 0 until 300
        } yield {
          if (rect contains V2F(x, y))
            buffer.setRGB(x, y, 0xFF0000FF)
          else if (prox contains V2F(x, y))
            buffer.setRGB(x, y, 0x00FF00FF)
          else
            buffer.setRGB(x, y, 0x0000FFFF)
        }
        g.drawImage(buffer, 0, 0, null)

        clicked match {
          case Some(v1) =>
            val v2 = prox.closestPerimiterPoint(v1)
            g.setColor(Color.black)
            g.drawLine(v1.x.toInt, v1.y.toInt, v2.x.toInt, v2.y.toInt)
          case None =>
        }
      }
    }

    def onMouse(e: MouseEvent): Unit = {
      clicked = Some(V2F(e.getX, e.getY))
      panel.revalidate()
      panel.repaint()
    }

    val listener = new MouseAdapter {
      override def mouseClicked(e: MouseEvent): Unit = onMouse(e)

      override def mouseDragged(e: MouseEvent): Unit = onMouse(e)
    }

    panel.setPreferredSize(new Dimension(300, 300))
    panel.addMouseListener(listener)
    panel.addMouseMotionListener(listener)

    val frame = new JFrame()
    frame.setContentPane(panel)
    frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
    frame.pack()
    frame.setLocationRelativeTo(null)
    frame.setVisible(true)
  }

}
