import javax.sound.sampled._
import java.io.File._
//import java.net.URL._
//import java.net.URLClassLoader._


object TestSound  {
  //Input:   scala TestSound -10
  def main(args: Array[String]): Unit = {
    val oneMinute = 60*1000

    // usage
    if (args.length < 1) showUsageAndExit

    // initialize the values from the user input
    val minutesToWait = args(0).toInt
    val gainControl = if (args.length==2) args(1).toInt else -20

    println(s"Timer started. Wait time is $minutesToWait minutes.\n")

    // wait the desired time
    for (i <- 1 to minutesToWait) {
      Thread.sleep(oneMinute)
      println(s"time remaining: ${minutesToWait-i} ...")
    }

    //print current directory
    println(" Current directory is" + getCurrentDirectory)
    val  urls = urlses(getClass.getClassLoader)
    println(urls.filterNot(_.toString.contains("ivy")).mkString("\n"))

    // play the sound twice
    for (i <- 1 to 2) {
      playSoundfile("notify.wav",gainControl) //had this wav in same directory as compile class file output
      Thread.sleep(7*1000)
    }
  }
  //current directory
  def getCurrentDirectory =  new java.io.File(".").getCanonicalPath

  def playSoundfile(f: String, gainControl: Int) {
    //val audioInputStream = AudioSystem.getAudioInputStream(new java.io.File(f))
    val audioInputStream = AudioSystem.getAudioInputStream(getClass.getResourceAsStream("notify.wav"))
    val clip = AudioSystem.getClip
    clip.open(audioInputStream)
    val floatGainControl = clip.getControl(FloatControl.Type.MASTER_GAIN).asInstanceOf[FloatControl]
    floatGainControl.setValue(gainControl) //reduce volume by x decibels (like -10f or -20f)
    clip.start
  }

  def showUsageAndExit {
    Console.err.println("Usage: timer minutes-before-alarm <gain-control>")
    Console.err.println(" gain-control should be something like -10 or -20")
    System.exit(1)
  }

  def urlses(cl: ClassLoader): Array[java.net.URL] = cl match {
      case null => Array()
      case u: java.net.URLClassLoader => u.getURLs() ++ urlses(cl.getParent)
      case _ => urlses(cl.getParent)
    }

}
