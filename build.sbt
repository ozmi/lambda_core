name := "lambda_core"

version := "1.0"

scalaVersion := "2.11.2"

// --- Dependencies ---
resolvers ++= Seq (
  "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/",
  "spray repo" at "http://repo.spray.io"
)

libraryDependencies ++= Seq (
  "org.scalameta"           %% "scalameta"          % "1.4.0",
  "org.scalatest"           %% "scalatest"          % "2.2.0"       % "test",
  "org.scalacheck"          %% "scalacheck"         % "1.12.2"      % "test"
)
