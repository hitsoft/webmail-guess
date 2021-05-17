organization := "com.hitsoft"

name := "webmail-guess"

scalaVersion := "2.11.4"

scalacOptions in Test ++= Seq("-Yrangepos")

libraryDependencies ++= Seq(
  "org.specs2" %% "specs2" % "2.3.13" % "test"
)
