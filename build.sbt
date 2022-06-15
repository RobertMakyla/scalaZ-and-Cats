name := "scalaz-and-cats-playground"
version := "1.0"

scalaVersion := "2.12.10"

val scalazVersion = "7.1.16"
val catsVersion = "2.4.2"

val catsCore = "org.typelevel" %% "cats-core" % catsVersion
val catsFree = "org.typelevel" %% "cats-free" % catsVersion
val catsLaws = "org.typelevel" %% "cats-laws" % catsVersion
val catsMtl = "org.typelevel" %% "cats-mtl-core" % "0.7.1"
val simulacrum = "org.typelevel" %% "simulacrum" % "1.0.1"
val kindProjector = compilerPlugin("org.typelevel" % "kind-projector" % "0.11.3" cross CrossVersion.full)
val resetAllAttrs = "org.scalamacros" %% "resetallattrs" % "1.0.0"
val munit = "org.scalameta" %% "munit" % "0.7.22"
val disciplineMunit = "org.typelevel" %% "discipline-munit" % "1.0.6"


libraryDependencies ++= {
  val scalaTestVersion  = "3.0.1"
  val scalaCheckVersion = "1.13.4"

  Seq(
    "org.scalatest"  %% "scalatest"  % scalaTestVersion % "test",
    "org.scalacheck" %% "scalacheck" % scalaCheckVersion % "test",
    "org.scalaz" %% "scalaz-core" % scalazVersion,
    "org.scalaz" %% "scalaz-effect" % scalazVersion,
    "org.scalaz" %% "scalaz-typelevel" % scalazVersion,
    "org.scalaz" %% "scalaz-scalacheck-binding" % scalazVersion % "test",
    catsCore,
    catsFree,
    catsMtl,
    simulacrum,
    kindProjector,
    resetAllAttrs,
    catsLaws % Test,
    munit % Test,
    disciplineMunit % Test

  )
}
scalacOptions ++= Seq(
  "-deprecation",
  "-encoding", "UTF-8",
  "-feature",
  "-language:_"
)