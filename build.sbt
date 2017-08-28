name := "scalasql"

version := "0.1"

scalaVersion := "2.12.3"

scalacOptions in Test ++= Seq("-Yrangepos")

libraryDependencies ++= Seq(
  "org.specs2" %% "specs2-core" % "3.8.9" % Test,
  "org.specs2" %% "specs2-mock" % "3.8.9" % Test
)
