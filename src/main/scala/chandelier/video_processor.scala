package chandelier

import java.io.File

import org.jcodec.api.awt.AWTSequenceEncoder
import org.jcodec.common.model.{Picture, Rational}
import org.jcodec.scale.AWTUtil

object VideoProcessor extends App {
/*
  val file = new File("/Users/mauricio/Downloads/water540.mp4")
  val grab = FrameGrab.createFrameGrab(NIOUtils.readableChannel(file))
  var picture: Picture = grab.getNativeFrame
  while (picture != null) {
    System.out.println(picture.getWidth + "x" + picture.getHeight + " " + picture.getColor)
    picture = grab.getNativeFrame
  }
*/
  import org.jcodec.api.FrameGrab
  import org.jcodec.common.io.NIOUtils

  val startSec = 1.0
  val frameCount = 200
  val file = new File("/Users/mauricio/Downloads/water540.mp4")

  val grab = FrameGrab.createFrameGrab(NIOUtils.readableChannel(file))
  grab.seekToSecondPrecise(startSec)

  val out = NIOUtils.writableFileChannel("/Users/mauricio/Downloads/water540_new2.mp4")
  val encoder: AWTSequenceEncoder = new AWTSequenceEncoder(out, Rational.R(25, 1));
  var i = 0
  for (i <- 0 to frameCount) {
    val picture: Picture = grab.getNativeFrame
    System.out.println(picture.getWidth + "x" + picture.getHeight + " " + picture.getColor)
    val data: Array[Array[Byte]] = picture.getData.clone
    val hShrinkFactor = 8
    val vShrinkFactor = 8
    val outputLines = picture.getHeight / vShrinkFactor
    val newData: Array[Array[Byte]] = new Array[Array[Byte]](3)
    newData(0) = new Array[Byte](picture.getWidth / hShrinkFactor * outputLines)
    newData(1) = new Array[Byte](picture.getWidth / hShrinkFactor * outputLines)
    newData(2) = new Array[Byte](picture.getWidth / hShrinkFactor * outputLines)
    for (j <- 0 to outputLines - 1) {
      for (k <- 0 to picture.getWidth / hShrinkFactor - 1) {
        var red = 0
        var green = 0
        var blue = 0
        for (m <- 0 to hShrinkFactor - 1) {
          for (n <- 0 to vShrinkFactor - 1) {
            red += data(0)(k * hShrinkFactor + j * picture.getWidth + m + n * picture.getWidth)
            green += data(1)(k * hShrinkFactor + j * picture.getWidth + m + n * picture.getWidth)
            blue += data(2)(k * hShrinkFactor + j * picture.getWidth + m + n * picture.getWidth)
          }
        }
        newData(0)(k + j * picture.getWidth / hShrinkFactor) = (red / hShrinkFactor / vShrinkFactor).asInstanceOf[Byte]
        newData(1)(k + j * picture.getWidth / hShrinkFactor) = (green / hShrinkFactor / vShrinkFactor).asInstanceOf[Byte]
        newData(2)(k + j * picture.getWidth / hShrinkFactor) = (blue / hShrinkFactor / vShrinkFactor).asInstanceOf[Byte]
      }
    }
    val newPicture: Picture = Picture.createPicture(picture.getWidth / hShrinkFactor, outputLines, newData, picture.getColor)
    val bufferedImage = AWTUtil.toBufferedImage(newPicture)
    encoder.encodeImage(bufferedImage)
  }
  encoder.finish()

}