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
  "org.scalatest" %% "scalatest" % "3.0.1" % "test" 
)
