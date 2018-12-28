ThisBuild / scalaVersion := "2.12.8"

scalaSource in Compile := baseDirectory.value / "src"

lazy val xcgProductionAnalysis = (project in file("."))
  .settings(
    Seq(
      name := "xcg-production-analysis",
      libraryDependencies ++= Seq(
        "org.scala-lang.modules" %% "scala-xml" % "1.1.1"
      )
    )
  )
