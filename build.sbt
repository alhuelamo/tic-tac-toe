val scala3Version = "3.0.0"

lazy val root = project
  .in(file("."))
  .settings(
    name := "tic-tac-toe",
    version := "1.0.0",
    organization := "alhuelamo",

    scalaVersion := scala3Version,
  )
