#!/bin/sh
SCALAZ_JAR=/Users/g/.ivy2/cache/org.scalaz/scalaz-core_2.11.0-RC3/jars/scalaz-core_2.11.0-RC3-7.0.6.jar
exec scala -classpath $SCALAZ_JAR -savecompiled -feature "$0" "$@"
!#

import scala.language.higherKinds
import scalaz._
import Scalaz._

sealed trait Action[+N]

type Args = Seq[String]

object Action {
  case class Clean[N](args: Args, next: N) extends Action[N]
  case object Done extends Action[Nothing]

  def clean(args: Args): Free[Action, Unit] = liftF[Action, Unit](Clean(args, ()))
  def done(): Free[Action, Unit] = liftF[Action, Unit](Done)

  implicit val actionToFunctor: Functor[Action] = new Functor[Action] {
    def map[A, B](fa: Action[A])(f: A => B): Action[B] = fa match {
      case c: Clean[A] => Clean(c.args, f(c.next))
      case Done => Done
    }
  }

  private def liftF[F[+_]: Functor, R](command: F[R]): Free[F, R] =
    Free.Suspend[F, R](Functor[F].map(command) { Free.Return[F, R](_) })
}

object Maven {
  import Action._
  def showProgram[R: Show](p: Free[Action, R]): String =
    p.resume.fold({
      case Clean(a, next) =>
        "mvn clean " + a.mkString(" ") + "\n" + showProgram(next)
      case Done => ""
    },
    { r: R => "return " + Show[R].shows(r) + "\n" })
}

// Script

import Action._

val program = for {
  _ <- clean(List())
  _ <- done()
} yield ()
print(Maven.showProgram(program))
