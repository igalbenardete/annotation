import sbt.Keys.{scalaVersion, scalacOptions}

val circeVersion = "0.8.0"

lazy val circeDependencies = Seq(
  "io.circe" %% "circe-core",
  "io.circe" %% "circe-generic",
  "io.circe" %% "circe-parser"
).map(_ % circeVersion)

lazy val commonSettings = Seq(
  name := "Annotation",
  version := "1.0",
  scalaVersion := "2.12.2",
  scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature"),
  resolvers += Resolver.sonatypeRepo("releases")
)

lazy val macroAnnotationSettings = Seq(
  addCompilerPlugin("org.scalameta" % "paradise" % "3.0.0-M9" cross CrossVersion.full),
    scalacOptions += "-Xplugin-require:macroparadise",
  scalacOptions in (Compile, console) ~= (_ filterNot (_ contains "paradise"))
)

lazy val projectThatDefinesMacroAnnotations = project.in(file("annotation-definition"))
  .settings(commonSettings)
  .settings(
    name := "HalResource",
    libraryDependencies += "org.scalameta" %% "scalameta" % "1.8.0" % Provided,
    macroAnnotationSettings)

lazy val annotation = project.in(file("."))
  .settings(commonSettings)
  .settings(macroAnnotationSettings)
  .settings(
    libraryDependencies ++= circeDependencies
  ).dependsOn(projectThatDefinesMacroAnnotations)
