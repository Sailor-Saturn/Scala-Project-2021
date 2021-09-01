name := "1160663_1160826_1160941_G1_M1B"

version := "0.1"

scalaVersion := "3.0.0"

scalacOptions ++= Seq("-source:future", "-indent", "-rewrite")

// XMl
libraryDependencies += "org.scala-lang.modules" %% "scala-xml" % "2.0.0"

// Scalatest
libraryDependencies += "org.scalactic" %% "scalactic" % "3.2.9"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.9" % "test"

// Scalacheck
libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.15.4" % "test"
