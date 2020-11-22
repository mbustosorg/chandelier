package chandelier

import java.io.File
import java.util.{Observable, Observer}

import chandelier.VideoProcessor.startSec
import com.illposed.osc.messageselector.OSCPatternAddressMessageSelector
import com.illposed.osc.{MessageSelector, OSCMessageEvent, OSCMessageListener}
import org.jcodec.api.FrameGrab
import org.jcodec.common.io.NIOUtils
import org.jcodec.common.model.Picture


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

  // Color temperatures from http://planetpixelemporium.com/tutorialpages/light.html
  case class ColorForTemp(red: Float, green: Float, blue: Float)
  val Candle = ColorForTemp(255, 147, 41)
  val W40Tungsten = ColorForTemp(255, 197, 143)
  val W100Tungsten = ColorForTemp(255, 214, 170)
  val Halogen = ColorForTemp(255, 251, 224)
  val CarbonArc = ColorForTemp(255, 250, 244)
  val HighNoon = ColorForTemp(255, 255, 251)
  val DirectSunlight = ColorForTemp(255, 255, 255)
  val OvercastSky = ColorForTemp(201, 226, 255)
  val ClearBlueSky = ColorForTemp(64, 156, 255)

  var red = 1.0
  var green = 1.0
  var blue = 1.0
  var hue = 0.0

  def setup (): Unit = {
    registry.addObserver (testObserver_1)
  }

  val file = new File("/Users/mauricio/Downloads/water540.mp4")
  val grab = FrameGrab.createFrameGrab(NIOUtils.readableChannel(file))
  grab.seekToSecondPrecise(startSec)

  def nextFrame(): Array[Array[Byte]] = {
    val picture: Picture = grab.getNativeFrame
    picture.getData
  }

  protected def colorCalc(x: Double, y: Double, z: Double, a: Double): Int = {

    val colorModeX = 255.0
    val colorModeY = 255.0
    val colorModeZ = 255.0
    val colorModeA = 255.0
    //val colorMode = "HSB"
    val colorMode = "RGB"
    val colorModeScale = true

    var calcR = x.min(colorModeX).max(0.0)
    var calcG = y.min(colorModeY).max(0.0)
    var calcB = z.min(colorModeZ).max(0.0)
    var calcA = a.min(colorModeA).max(0.0)

    colorMode match {
      case "RGB" =>
        if (colorModeScale) {
          calcR = x / colorModeX
          calcG = y / colorModeY
          calcB = z / colorModeZ
          calcA = a / colorModeA
        }
        else {
          calcR = x
          calcG = y
          calcB = z
          calcA = a
        }
      case "HSB" =>
        val x1 = x / colorModeX // h
        val y1 = y / colorModeY // s
        val z1 = z / colorModeZ // b

        calcA = a / colorModeA
//        calcA = if (colorModeScale) a / colorModeA
//        else a
//        if (y eq 0) { // saturation == 0
//          calcR = calcG = calcB = z
//        }
        val which = (x1 - x1.asInstanceOf[Int]) * 6.0f
        val f = which - which.toInt
        val p = z1 * (1.0f - y1)
        val q = z1 * (1.0f - y1 * f)
        val t = z1 * (1.0f - (y1 * (1.0f - f)))
        which.toInt match {
          case 0 =>
            calcR = z1
            calcG = t
            calcB = p
          case 1 =>
            calcR = q
            calcG = z1
            calcB = p
          case 2 =>
            calcR = p
            calcG = z1
            calcB = t
          case 3 =>
            calcR = p
            calcG = q
            calcB = z1
          case 4 =>
            calcR = t
            calcG = p
            calcB = z1
          case 5 =>
            calcR = z1
            calcG = p
            calcB = q
        }
    }
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
/*
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
*/
      hue += 1.0
      if (hue > 255.0) {hue = 0.0}
      for (strip <- strips.asScala) {
        val xscale = 100 / strip.getLength
        var stripx = 0
        while ( {
          stripx < strip.getLength
        }) {
          //strip.setPixel (colorCalc(hue, 255.0, 255.0, 255.0), stripx)
          strip.setPixel (colorCalc(red, green, blue, 255.0), stripx)
          stripx += 1
          stripx - 1
        }
        stripy += 1
      }
    }
  }

  import com.illposed.osc.transport.udp.OSCPortIn

  val receiver: OSCPortIn = new OSCPortIn(1234)
  val listener = new OSCMessageListener {
    override def acceptMessage(event: OSCMessageEvent): Unit = {
      if (event.getMessage.getAddress == "/red") {
        red = event.getMessage.getArguments.get(0).asInstanceOf[Float]
      } else if (event.getMessage.getAddress == "/green") {
        green = event.getMessage.getArguments.get(0).asInstanceOf[Float]
      } else if (event.getMessage.getAddress == "/blue") {
        blue = event.getMessage.getArguments.get(0).asInstanceOf[Float]
      }
    }
  }
  val selector_red: MessageSelector = new OSCPatternAddressMessageSelector("/red")
  val selector_green: MessageSelector = new OSCPatternAddressMessageSelector("/green")
  val selector_blue: MessageSelector = new OSCPatternAddressMessageSelector("/blue")
  receiver.getDispatcher.addListener(selector_red, listener)
  receiver.getDispatcher.addListener(selector_green, listener)
  receiver.getDispatcher.addListener(selector_blue, listener)
  receiver.startListening

  setup()
  while(true) {
    Thread.sleep(50)
    draw()
  }
}

