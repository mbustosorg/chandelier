package pixel_test

import java.util.{Observable, Observer}

class TestObserver extends Observer {
  var hasStrips = false

  override def update(registry: Observable, updatedDevice: Any): Unit = {
    println("Registry changed!")
    if (updatedDevice != null) println("Device change: " + updatedDevice)
    this.hasStrips = true
  }
}

object PixelTest extends App {
  import com.heroicrobot.dropbit.registry.DeviceRegistry

  val registry: DeviceRegistry = new DeviceRegistry
  val testObserver_1: TestObserver = new TestObserver

  import scala.collection.JavaConverters._

  var red = 1.0
  var green = 1.0
  var blue = 1.0

  def setup (): Unit = {
    registry.addObserver (testObserver_1)
  }

  protected def colorCalc(x: Double, y: Double, z: Double, a: Double): Int = {

    val colorModeX = 255.0
    val colorModeY = 255.0
    val colorModeZ = 255.0
    val colorModeA = 255.0
    val calcR = x.min(colorModeX).max(0.0) / colorModeX
    val calcG = y.min(colorModeY).max(0.0) / colorModeY
    val calcB = z.min(colorModeZ).max(0.0) / colorModeZ
    val calcA = a.min(colorModeA).max(0.0) / colorModeA
    val calcRi = (255.0 * calcR).asInstanceOf[Int]
    val calcGi = (255.0 * calcG).asInstanceOf[Int]
    val calcBi = (255.0 * calcB).asInstanceOf[Int]
    val calcAi = (255.0 * calcA).asInstanceOf[Int]
    (calcAi << 24) | (calcRi << 16) | (calcGi << 8) | calcBi
  }

  def draw (): Unit = {
    if (testObserver_1.hasStrips) {
      registry.startPushing
      val strips = registry.getStrips
      var stripy = 0
      val yscale = 100 / strips.size

      if (red < 255.0 && red >= 0.0) {
        red += 1
        green = 0.0
        blue = 0.0
      } else if (green < 255.0 && green >= 0.0) {
        red = 0.0
        green += 1
        blue = 0.0
      } else if (blue < 255.0 && blue >= 0.0) {
        red = 0.0
        green = 0.0
        blue += 1
      } else red = 0.0
      for (strip <- strips.asScala) {
        val xscale = 100 / strip.getLength
        var stripx = 0
        while ( {
          stripx < strip.getLength
        }) {
          strip.setPixel (colorCalc(red, green, blue, 255.0), stripx)
          stripx += 1
          stripx - 1
        }
        stripy += 1
      }
    }
  }

  setup()
  while(true) {
    Thread.sleep(50)
    draw()
  }
}

