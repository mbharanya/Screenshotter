import java.awt.Color
import java.awt.image.BufferedImage
import scala.util.control.Breaks._


class FastTrim(private val img: BufferedImage, private val color: Color) {
  def trim() = {
    val width = this.img.getWidth
    val height = this.img.getHeight

    var first_y = 0
    var first_x = 0
    var last_y = 0
    var last_x = 0

    breakable{
      for (x <- 0 until width) {
        val hasColorInLine = (0 until height).exists(y => img.getRGB(x, y) != this.color.getRGB)
        if (hasColorInLine) {
          first_x = x
          break()
        }
      }
    }
    breakable {
      for (y <- 0 until height) {
        val hasColorInColumn = (0 until width).exists(x => img.getRGB(x, y) != this.color.getRGB)
        if (hasColorInColumn) {
          first_y = y
          break()
        }
      }
    }

    breakable {
      for (x <- 0 until width) {
        val hasColorInLine = (0 until height).exists(y => img.getRGB(x, y) != this.color.getRGB)
        if (hasColorInLine) {
          first_x = x
          break()
        }
      }
    }

    breakable {
      for (y <- height until 0 by -1) {
        val hasColorInColumn = (width until 0 by -1).exists(x => img.getRGB(x - 1, y - 1) != this.color.getRGB)
        if (hasColorInColumn) {
          last_y = y
          break()
        }
      }
    }

    breakable {
      for (x <- width until 0 by -1) {
        val hasColorInLine = (height until 0 by -1).exists(y => img.getRGB(x - 1, y - 1) != this.color.getRGB)
        if (hasColorInLine) {
          last_x = x
          break()
        }
      }
    }

   img.getSubimage(first_x, first_y, last_x - first_x, last_y - first_y)
  }
}
