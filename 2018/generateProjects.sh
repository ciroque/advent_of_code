#!/bin/bash

SRC_PATH=src/main/scala/org/ciroque/AoC

writeBuildSbt() {
  cat > build.sbt <<-EOF
lazy val root = (project in file(".")).
settings(
  inThisBuild(List(
    organization := "org.ciroque",
    scalaVersion := "2.12.7",
    version      := "0.1.0-SNAPSHOT"
  )),
  name := "AoC"
)

EOF
}

writeAppFile() {
  cat > AoC.scala <<-EOF
package org.ciroque

object AoC extends Data with App {
  println("...Running")
}

object Solution {
}

trait Data {
}

EOF
}

createDirectories() {
  mkdir -p $SRC_PATH
}

main() {
  for d in {3..25}
  do
    pushd $d
    writeBuildSbt
    createDirectories
    pushd $SRC_PATH
    writeAppFile
    popd
    popd
  done
}

main

