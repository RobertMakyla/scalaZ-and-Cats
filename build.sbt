name := "scalaz-and-cats-playground"
version := "1.0"

scalaVersion := "2.11.2"

val scalazVersion = "7.1.0"

libraryDependencies ++= {
  val scalaTestVersion  = "3.0.1"
  val scalaCheckVersion = "1.13.4"

  Seq(
    "org.scalatest"  %% "scalatest"  % scalaTestVersion % "test",
    "org.scalacheck" %% "scalacheck" % scalaCheckVersion % "test",
    "org.scalaz" %% "scalaz-core" % scalazVersion,
    "org.scalaz" %% "scalaz-effect" % scalazVersion,
    "org.scalaz" %% "scalaz-typelevel" % scalazVersion,
    "org.scalaz" %% "scalaz-scalacheck-binding" % scalazVersion % "test"

  )
}