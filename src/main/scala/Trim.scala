import java.awt.Color
import java.awt.image.BufferedImage

import scala.util.control.Breaks._


class Trim(private val img: BufferedImage, private val color: Color) {
  def trim() = {
    val width = this.img.getWidth
    val height = this.img.getHeight

     val (x_max, x_min, y_max, y_min) = (
        for {
          x <- 0 until width
          y <- 0 until height
        } yield (x, y)
        )
        .par
        .filter { case (x, y) => img.getRGB(x, y) != this.color.getRGB }
        .foldLeft(0,Int.MaxValue,0,Int.MaxValue){
          (values, element) => values match {
            case (x_max, x_min, y_max, y_min) =>
              (
                Math.max(x_max, element._1),
                Math.min(x_min, element._1),
                Math.max(y_max, element._2),
                Math.min(y_min, element._2)
              )
          }
        }
   img.getSubimage(x_min, y_min, x_max - x_min, y_max - y_min)
  }
}
