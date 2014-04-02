#!/bin/sh
exec scala -savecompiled "$0" "$@"
!#

import java.io.{IOException, PrintStream, InputStream}
import scala.util.{Success, Failure, Try}

type Recipe = Array[String]

execute(Array("java", "-version"))

def execute(recipe: Recipe): Try[Unit] = {
  val builder = new ProcessBuilder(recipe : _*)
  builder.redirectErrorStream(true)
  val process = builder.start()
  val stdout = process.getInputStream
  val consoleConsumer = new ConsoleConsumer(stdout, java.lang.System.out)
  val outThread = new Thread(consoleConsumer, "System")
  outThread.start()

  try {
    process.waitFor()
  } catch {
    case _: InterruptedException => process.destroy()
  }

  // This ensures that all output is complete
  // before returning (waitFor does not ensure this)
  outThread.join()

  if (process.exitValue() != 0)
    Failure(throw new Exception(s"[$recipe] execution failed in ${builder.directory()}"))
  else
    Success()
}

private class ConsoleConsumer(source: InputStream, target: PrintStream) extends Runnable {
  def run() {
    val source = this.source
    try {
      val buf = new Array[Byte](32)
      // Do not try reading a line cos it considers '\r' end of line
      var reading = true
      while (reading) {
        source.read(buf) match {
          case -1 => reading = false
          case c => target.write(buf, 0, c)
        }
      }
    } catch {
      case e: IOException => e.printStackTrace(target)
    } finally {
      source.close()
    }
  }
}
