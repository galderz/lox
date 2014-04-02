name := "lox"

version := "0.1.0"

scalaVersion := "2.11.0-RC3"

scalacOptions ++= Seq("-deprecation", "-unchecked", "-feature")

libraryDependencies ++= Seq(
  "org.scalaz"        %% "scalaz-core"  % "7.0.6",
  "org.scalatest"     %% "scalatest"    % "2.1.2"   % "test"
)
