package chandelier

import java.io.File
import java.util.{Observable, Observer}

import chandelier.VideoProcessor.startSec
import com.illposed.osc.messageselector.OSCPatternAddressMessageSelector
import com.illposed.osc.{OSCMessageEvent, OSCMessageListener}
import org.jcodec.api.FrameGrab
import org.jcodec.common.io.NIOUtils
import org.jcodec.common.model.{ColorSpace, Picture}
import org.jcodec.scale.ColorUtil


class TestObserver extends Observer {
  var hasStrips = false

  override def update(registry: Observable, updatedDevice: Any): Unit = {
    if (updatedDevice != null) println("Device change: " + updatedDevice)
    this.hasStrips = true
  }
}

object DisplayMode extends Enumeration {
  type Main = Value

  val VIDEO = Value("VIDEO")
  val RGB = Value("RGB")
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

  var mode: DisplayMode.Value = DisplayMode.VIDEO
  var red = 1.0
  var green = 1.0
  var blue = 1.0
  var hue = 0.0

  def setup (): Unit = {
    registry.addObserver (testObserver_1)
  }

  val file = new File("/Users/mauricio/Downloads/water540_new2.mp4")
  var grab = FrameGrab.createFrameGrab(NIOUtils.readableChannel(file))
  grab.seekToSecondPrecise(startSec)

  def newGrab(): Picture = {
    val file = new File("/Users/mauricio/Downloads/water540_new2.mp4")
    grab = FrameGrab.createFrameGrab (NIOUtils.readableChannel (file) )
    grab.seekToSecondPrecise (startSec)
    grab.getNativeFrame
  }

  val transform = ColorUtil.getTransform(ColorSpace.YUV420, ColorSpace.RGB)

  def nextFrame(): Array[Array[Byte]] = {

    val picture = {
      try {
        val result = grab.getNativeFrame
        if (result == null) newGrab else result
      } catch {
        case _: Throwable => newGrab
      }
    }
    if (picture.getColor != ColorSpace.RGB) {
      val rgb = Picture.create(picture.getWidth, picture.getHeight, ColorSpace.RGB)
      transform.transform(picture, rgb)
      rgb.getData
    } else {
      picture.getData
    }
  }

  protected def colorCalc(x: Double, y: Double, z: Double, a: Double, colorMode: String): Int = {

    val colorModeX = 255.0
    val colorModeY = 255.0
    val colorModeZ = 255.0
    val colorModeA = 255.0
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
      val nextData: Array[Array[Byte]] = nextFrame
      val strips = registry.getStrips
      var stripy = 0
      val yscale = 100 / strips.size
      for (strip <- strips.asScala) {
        val xscale = 100 / strip.getLength
        var stripx = 0
        for (stripx <- 0 to strip.getLength - 1) {
          //strip.setPixel (colorCalc(hue, 255.0, 255.0, 255.0, "HSB"), stripx)
          if (mode == DisplayMode.VIDEO) {
            val red = (nextData(0)(stripx * 3 + 0) + 128).asInstanceOf[Float]
            val green = (nextData(0)(stripx * 3 + 1) + 128).asInstanceOf[Float]
            val blue = (nextData(0)(stripx * 3 + 2) + 128).asInstanceOf[Float]
            strip.setPixel(colorCalc(red, green, blue, 255.0, "RGB"), stripx)
          } else {
            strip.setPixel(colorCalc(red, green, blue, 255.0, "RGB"), stripx)
          }
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
        mode = DisplayMode.RGB
      } else if (event.getMessage.getAddress == "/green") {
        green = event.getMessage.getArguments.get(0).asInstanceOf[Float]
        mode = DisplayMode.RGB
      } else if (event.getMessage.getAddress == "/blue") {
        blue = event.getMessage.getArguments.get(0).asInstanceOf[Float]
        mode = DisplayMode.RGB
      } else if (event.getMessage.getAddress == "/lamp") {
        mode = DisplayMode.RGB
        val index = event.getMessage.getArguments.get(0).asInstanceOf[Float].toInt
        index match {
          case 0 => {
            red = Candle.red
            green = Candle.green
            blue = Candle.blue
          }
          case 1 => {
            red = W40Tungsten.red
            green = W40Tungsten.green
            blue = W40Tungsten.blue
          }
          case 2 => {
            red = W100Tungsten.red
            green = W100Tungsten.green
            blue = W100Tungsten.blue
          }
          case 3 => {
            red = Halogen.red
            green = Halogen.green
            blue = Halogen.blue
          }
          case 4 => {
            red = CarbonArc.red
            green = CarbonArc.green
            blue = CarbonArc.blue
          }
          case 5 => {
            red = HighNoon.red
            green = HighNoon.green
            blue = HighNoon.blue
          }
          case 6 => {
            red = DirectSunlight.red
            green = DirectSunlight.green
            blue = DirectSunlight.blue
          }
          case 7 => {
            red = OvercastSky.red
            green = OvercastSky.green
            blue = OvercastSky.blue
          }
          case 8 => {
            red = ClearBlueSky.red
            green = ClearBlueSky.green
            blue = ClearBlueSky.blue
          }
        }
      } else if (event.getMessage.getAddress == "/video") {
        mode = DisplayMode.VIDEO
      }
    }
  }
  receiver.getDispatcher.addListener(new OSCPatternAddressMessageSelector("/red"), listener)
  receiver.getDispatcher.addListener(new OSCPatternAddressMessageSelector("/green"), listener)
  receiver.getDispatcher.addListener(new OSCPatternAddressMessageSelector("/blue"), listener)
  receiver.getDispatcher.addListener(new OSCPatternAddressMessageSelector("/lamp"), listener)
  receiver.getDispatcher.addListener(new OSCPatternAddressMessageSelector("/video"), listener)
  receiver.startListening

  setup()
  while(true) {
    Thread.sleep(16)
    draw()
  }
}

