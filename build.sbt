scalaVersion := "2.13.6"

resolvers ++= Seq(
  "jitpack" at "https://jitpack.io"
)

libraryDependencies ++= Seq(
  "com.lihaoyi"   %% "fastparse"        % "2.2.2",
  "org.scalameta" %% "munit"            % "0.7.26" % Test,
  "org.scalameta" %% "munit-scalacheck" % "0.7.26" % Test,
  "com.github.tchudyk" %% "pocket-integration" % "1.2.0"
)

scalacOptions ++= Seq(
  "-encoding", "utf8",
  "-Xlint:implicit-recursion",
  "-Xfatal-warnings",
  "-deprecation",
  "-unchecked",
  "-P:semanticdb:sourceroot:/Users/vgerasimov/Projects/org4s"
)

lazy val root = project
  .in(file("."))
  .settings(
    name := "org4s",
    organization := "dev.vgerasimov",
    version := "0.1.0",
    idePackagePrefix := Some("dev.vgerasimov.org4s"),
    testOptions += Tests.Argument(
        framework = Some(new TestFramework("munit.Framework")),
        args = List("-oSD")
      ),
    githubOwner := "wlad031",
    githubRepository := "org4s"
  )


