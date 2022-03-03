scalaVersion := "2.13.8"

scalacOptions ++= Seq("-deprecation", "-feature", "-unchecked", "-Xlint")

libraryDependencies += "org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.4"
