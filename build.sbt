

name := "chandelier"

version := "0.1"

scalaVersion := "2.12.6"

unmanagedBase := baseDirectory.value / "lib"

libraryDependencies += "com.illposed.osc" % "javaosc-core" % "0.7"
libraryDependencies += "org.jcodec" % "jcodec" % "0.2.5"
libraryDependencies += "org.jcodec" % "jcodec-javase" % "0.2.5"
