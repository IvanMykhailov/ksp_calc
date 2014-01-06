name := "ksp_calc"

organization := "strider.van"

version := "0.1-SNAPSHOT"

//libraryDependencies ++= Seq(
//  "com.spatial4j" % "spatial4j" % "0.3",
//  "org.specs2" % "specs2_2.10" % "2.0-RC2" % "test",
//  "com.github.scala-incubator.io" % "scala-io-core_2.10" % "0.4.2"
//)

libraryDependencies += "org.specs2" % "specs2_2.10" % "2.3.6"

scalacOptions ++= Seq(
    "-deprecation"
  , "-feature"
  , "-unchecked"
  , "-Xlint"
  , "-Yno-adapted-args"
  , "-Ywarn-all"
  , "-Ywarn-dead-code"
  , "-language:postfixOps"
  , "-language:implicitConversions"
)
