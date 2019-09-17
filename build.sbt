name := "ScalaPlayground"

version := "0.1"

scalaVersion := "2.12.9"

resolvers += "Artima Maven Repository" at "http://repo.artima.com/releases"

libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.8"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.8" % "test"


addSbtPlugin("com.artima.supersafe" % "sbtplugin" % "1.1.3")