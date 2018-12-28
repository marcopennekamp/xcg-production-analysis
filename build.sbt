ThisBuild / scalaVersion := "2.12.8"

scalaSource in Compile := baseDirectory.value / "src"

lazy val xcgProductionAnalysis = (project in file("."))
  .settings(
    Seq(
      name := "xcg-production-analysis",
      libraryDependencies ++= Seq(
        "org.scala-lang.modules" %% "scala-xml" % "1.1.1",
        "io.circe" %% "circe-core" % "0.10.1",
        "io.circe" %% "circe-generic" % "0.10.1",
        "io.circe" %% "circe-yaml" % "0.9.0",
        "com.lihaoyi" %% "scalatags" % "0.6.7",
      )
    )
  )
