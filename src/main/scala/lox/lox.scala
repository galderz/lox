#!/bin/sh
SCALAZ_JAR=/Users/g/.ivy2/cache/org.scalaz/scalaz-core_2.11.0-RC3/jars/scalaz-core_2.11.0-RC3-7.0.6.jar
exec scala -classpath $SCALAZ_JAR -savecompiled -feature "$0" "$@"
!#

import java.io.{IOException, PrintStream, InputStream}
import scala.collection.mutable.ListBuffer
import scalaz.{~>,Id,Free,Functor}, Free.Return, Free.Suspend, Id.Id
import scala.language.implicitConversions

import Process.process
import Environment.env

val cmdargs = args.toList

cmdargs match {
  case Nil =>
    println("Run Lox test suite")
  case x::xs =>
    x match {
      case "clear" =>
        val program: Free[EnvironmentF, Unit] =
          for {
            a <- env.clean(Array(""))
          } yield a

        Maven(program)
        //program.collect(exe.apply[Environment[A]])
    }
}

//val program: Free[ProcessF, Unit] =
//  for {
//    a <-
//  } yield a

// Basic tests
//OperativeSystem(process.execute(Array("java", "-version")))
// Maven(env.clean(Array("")))

// Supporting classes - Environment

sealed trait EnvironmentF[+A]

object Environment {
  type Environment[A] = Free[EnvironmentF, A]

  implicit def envFFunctor[B]: Functor[EnvironmentF] = new Functor[EnvironmentF]{
    def map[A,B](fa: EnvironmentF[A])(f: A => B): EnvironmentF[B] =
      fa match {
        case Clean(recipe,a) => Clean(recipe,f(a))
        case Build(recipe,a) => Build(recipe,f(a))
        case Test(recipe,a) => Test(recipe,f(a))
      }
  }

  implicit def envFToFree[A](logf: EnvironmentF[A]): Free[EnvironmentF,A] =
    Suspend[EnvironmentF, A](Functor[EnvironmentF].map(logf)(a => Return[EnvironmentF, A](a)))

  case class Clean[A](recipe: Recipe, o: A) extends EnvironmentF[A]
  case class Build[A](recipe: Recipe, o: A) extends EnvironmentF[A]
  case class Test[A](recipe: Recipe, o: A) extends EnvironmentF[A]

  object env {
    def clean(recipe: Recipe): Environment[Unit] = Clean(recipe, ())
    def build(recipe: Recipe): Environment[Unit] = Build(recipe, ())
    def test(recipe: Recipe): Environment[Unit] = Test(recipe, ())
  }

}

object Maven {
  import Environment._
  import Process.process

  private def execute(recipe: Recipe): Unit = OperativeSystem(process.execute(recipe))

  private def clean(recipe: Recipe): Unit = execute(Array("mvn", "clean"))
  private def build(recipe: Recipe): Unit = execute(Array("mvn", "-Dmaven.test.skip.exec=true", "install"))
  private def test(recipe: Recipe): Unit = execute(Array("mvn", "install"))

  private val exe: EnvironmentF ~> Id = new (EnvironmentF ~> Id) {
    def apply[B](l: EnvironmentF[B]): B = l match {
      case Clean(recipe,a) => { clean(recipe); a }
      case Build(recipe,a) => { build(recipe); a }
      case Test(recipe,a) => { test(recipe); a }
    }
  }

  def apply[A](log: Environment[A]): A =
    log.runM(exe.apply[Environment[A]])

}

// Supporting classes - Process execution

sealed trait ProcessF[+A]

type Recipe = Array[String]

object Process {
  type Process[A] = Free[ProcessF, A]

  implicit def processFFunctor[B]: Functor[ProcessF] = new Functor[ProcessF]{
    def map[A,B](fa: ProcessF[A])(f: A => B): ProcessF[B] =
      fa match {
        case Execute(msg,a) => Execute(msg,f(a))
      }
  }

  implicit def processFToFree[A](logf: ProcessF[A]): Free[ProcessF,A] =
    Suspend[ProcessF, A](Functor[ProcessF].map(logf)(a => Return[ProcessF, A](a)))

  case class Execute[A](recipe: Recipe, o: A) extends ProcessF[A]

  object process {
    def execute(recipe: Recipe): Process[Unit] = Execute(recipe, ())
  }
}

object DummySystem {
  import Process._

  val log = new ListBuffer[Recipe]

  private def execute(recipe: Recipe): Unit = {
    log += recipe
  }

  private val exe: ProcessF ~> Id = new (ProcessF ~> Id) {
    def apply[B](l: ProcessF[B]): B = l match {
      case Execute(recipe,a) => { execute(recipe); a }
    }
  }

  def apply[A](log: Process[A]): A =
    log.runM(exe.apply[Process[A]])

}

object OperativeSystem {
  import Process._

  private def execute(recipe: Recipe): Unit = {
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
      throw new Exception(s"[$recipe] execution failed in ${builder.directory()}")
  }

  private val exe: ProcessF ~> Id = new (ProcessF ~> Id) {
    def apply[B](l: ProcessF[B]): B = l match {
      case Execute(recipe,a) => { execute(recipe); a }
    }
  }

  def apply[A](log: Process[A]): A =
    log.runM(exe.apply[Process[A]])

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

}