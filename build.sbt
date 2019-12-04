name := "RadonC"

version := "0.1"

scalaVersion := "2.12.6"

//libraryDependencies += "org.mockito" % "mockito-core" % "2.28.2" % "test"
//libraryDependencies += "org.scalamock" %% "scalamock" % "4.1.0" % Test
libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.8"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.8" % "test"
libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.1"
// libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.2.27"
libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.2.28"
// libraryDependencies += "net.codingwell" %% "scala-guice" % "4.1.0"
libraryDependencies += "commons-lang" % "commons-lang" % "2.6"
libraryDependencies += "org.apache.bcel" % "bcel" % "6.0"

scalaSource in Compile := baseDirectory.value / "src"
scalaSource in Test := baseDirectory.value / "test"
