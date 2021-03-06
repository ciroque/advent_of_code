lazy val root = (project in file(".")).
settings(
  inThisBuild(List(
    organization := "org.ciroque",
    scalaVersion := "2.12.7",
    version      := "0.1.0-SNAPSHOT"
  )),
  name := "AoC"
)

libraryDependencies += "ar.com.hjg" % "pngj" % "2.1.0"
