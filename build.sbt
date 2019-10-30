name := "ScalaPlayground"

version := "0.1"

scalaVersion := "2.12.9"

resolvers += "Artima Maven Repository" at "http://repo.artima.com/releases"

libraryDependencies += "io.spray" %%  "spray-json" % "1.3.4"

libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.8"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.8" % "test"

libraryDependencies ++= Seq(
  "com.tethys-json" %% "tethys" % "0.10.0"
)

libraryDependencies += "com.tethys-json" %% "tethys-json4s" % "0.10.0"

addSbtPlugin("com.artima.supersafe" % "sbtplugin" % "1.1.3")