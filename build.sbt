scalaVersion := "2.13.5"

libraryDependencies += "com.lihaoyi" %% "fastparse" % "2.2.2"
libraryDependencies += "org.scalameta" %% "munit" % "0.7.26" % Test
libraryDependencies += "org.scalameta" %% "munit-scalacheck" % "0.7.26" % Test

scalacOptions ++= Seq(
  "-encoding",
  "utf8",
  "-Xlint:implicit-recursion",
  "-Xfatal-warnings",
  "-deprecation",
  "-unchecked"
)

lazy val root = project
  .in(file("."))
  .settings(
    name := "scorg",
    organization := "dev.vgerasimov",
    version := "0.1.0",
    idePackagePrefix := Some("dev.vgerasimov.scorg"),
    testOptions += Tests.Argument(
      framework = Some(new TestFramework("munit.Framework")),
      args = List("-oSD")
    )
  )

