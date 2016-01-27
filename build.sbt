name := "MusicalChairs_AkkaExcample"

version := "1.0"

scalaVersion := "2.11.7"

libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-actor" % "2.4.1"
)

mainClass in Compile := Some("main.Main")