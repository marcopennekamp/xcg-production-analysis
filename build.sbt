ThisBuild / scalaVersion := "2.12.8"

scalaSource in Compile := baseDirectory.value / "src"

lazy val xcgProductionAnalysis = (project in file("."))
  .settings(
    name := "xcg-production-analysis"
  )
