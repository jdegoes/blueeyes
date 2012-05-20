name := "blueeyes-actor"

libraryDependencies ++= Seq(
  "org.scalaz"                  %% "scalaz-core"        % "6.0.2",
  "com.typesafe.akka"           % "akka-actor"          % "2.0",
  "com.weiglewilczek.slf4s"     %% "slf4s"              % "1.0.7",
  "org.specs2"                  %% "specs2"             % "1.8"            % "test",
  "org.mockito"                 % "mockito-all"         % "1.8.5"          % "test",
  "org.scala-tools.testing"     %% "scalacheck"         % "1.9"            % "test"
)
