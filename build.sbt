name := "lambda_core"

version := "1.0"

scalaVersion := "2.11.2"

// --- Dependencies ---

libraryDependencies ++= Seq(
  "com.googlecode.kiama"    %% "kiama"              % "1.8.0",
  "org.scalatest"           %% "scalatest"          % "2.2.0"       % "test",
  "org.scalacheck"          %% "scalacheck"         % "1.12.2"      % "test"
)
