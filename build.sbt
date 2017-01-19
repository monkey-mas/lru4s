val Scala211 = "2.11.8"

scalaVersion := Scala211

crossScalaVersions := Scala211 :: "2.12.1" :: Nil

name := "LRU4s"

scalacOptions ++= (
  "-deprecation" ::
  "-unchecked" ::
  "-Xlint" ::
  "-language:existentials" ::
  "-language:higherKinds" ::
  "-language:implicitConversions" ::
  Nil
)

libraryDependencies ++= Seq(
  "com.google.guava" % "guava" % "21.0",
  "org.scalatest" %% "scalatest" % "3.0.1" % "test"
)
