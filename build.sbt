ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.3.0"

lazy val root = (project in file("."))
  .settings(
    name := "TreeManipulations"
  )

libraryDependencies ++= Seq(
  "dev.zio" %% "zio" % "2.0.15",
  "dev.zio" %% "zio-test"          % "2.0.15" % Test,
  "dev.zio" %% "zio-test-sbt"      % "2.0.15" % Test,
  "dev.zio" %% "zio-test-magnolia" % "2.0.15" % Test
)
testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework")