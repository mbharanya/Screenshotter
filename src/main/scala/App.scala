import java.awt.{GraphicsEnvironment, _}
import java.awt.image.BufferedImage
import java.io._
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import javax.imageio.ImageIO

object Screenshotter extends App {
  val startTime = LocalDateTime.now()
  val trimmer = new FastTrim(Color.BLACK)
  var lastUntrimmedDimensions: Option[Rectangle] = None
  var lastTrimmedDimension: Option[ResizeDimensions] = None

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

    val newDimensions = ScreenshotUtil.getDimensions

    if (!lastUntrimmedDimensions.contains(newDimensions) || lastTrimmedDimension.isEmpty){
      lastTrimmedDimension = Some(trimmer.getDimensions(bufferedImage))
    }
    lastUntrimmedDimensions = Some(newDimensions)

    val trimmed = trimmer.trim(lastTrimmedDimension.get, bufferedImage)

    val out = new FileOutputStream(file)
    ImageIO.write(trimmed, "png", out)
    out.close()
    val runtime = System.currentTimeMillis() - start
    println(s"Saved ${filename}, runtime: ${runtime} ms")
  }
}

object ScreenshotUtil {
  val robot = new Robot

  def getDimensions = {
    val ge = GraphicsEnvironment.getLocalGraphicsEnvironment
    val screens = ge.getScreenDevices
    var allScreenBounds = new Rectangle
    for (screen <- screens) {
      val screenBounds = screen.getDefaultConfiguration.getBounds
      allScreenBounds = allScreenBounds.union(screenBounds)
    }
    allScreenBounds
  }

  def allMonitors: BufferedImage = {
    robot.createScreenCapture(getDimensions)
  }
}



