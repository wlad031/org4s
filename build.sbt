scalaVersion := "3.1.2"

resolvers ++= Seq(
  "jitpack" at "https://jitpack.io"
)

libraryDependencies ++= Seq(
  "dev.vgerasimov" %% "slowparse"        % "0.1.3",
  "org.scalameta"  %% "munit"            % "0.7.26" % Test,
  "org.scalameta"  %% "munit-scalacheck" % "0.7.26" % Test,
)

scalacOptions ++= Seq(
  "-encoding", "utf8",
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
    githubRepository := "org4s",
    resolvers += Resolver.githubPackages("wlad031"),
  )


