import java.awt.{GraphicsEnvironment, _}
import java.awt.image.BufferedImage
import java.io._
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import javax.imageio.ImageIO

object Screenshotter extends App {
  val startTime = LocalDateTime.now()
  while(true){
    saveShot
    Thread.sleep(7* 1000 * 60)
  }

  private def saveShot = {
    val start = System.currentTimeMillis()
    val filename = LocalDateTime.now().format(DateTimeFormatter.ofPattern("yyyy-MM/yyyy-MM-dd-EEEE/yyyy-MM-dd'T'HH.mm.ss.'png'"))
    val file = new File(filename)
    file.getParentFile.mkdirs
    val bufferedImage = ScreenshotUtil.allMonitors
    val trimmer = new Trim(bufferedImage, Color.BLACK)
    val trimmed = trimmer.trim()

    val out = new FileOutputStream(file)
    ImageIO.write(trimmed, "png", out)
    out.close()
    val runtime = System.currentTimeMillis() - start
    println(s"Saved ${filename}, runtime: ${runtime} ms")
  }
}

object ScreenshotUtil {
  def allMonitors: BufferedImage = {
    val ge = GraphicsEnvironment.getLocalGraphicsEnvironment
    val screens = ge.getScreenDevices
    var allScreenBounds = new Rectangle
    for (screen <- screens) {
      val screenBounds = screen.getDefaultConfiguration.getBounds
      allScreenBounds = allScreenBounds.union(screenBounds)
    }
    println(allScreenBounds)
    val robot = new Robot
    robot.createScreenCapture(allScreenBounds)
  }
}



