name := "aoc"

version := "0.1"

scalaVersion := "2.13.1"

scalacOptions += "-target:jvm-1.8"

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.1.0" % "test",
  "org.apache.commons" % "commons-io" % "1.3.2"
)
