name := "blueeyes-actor"

libraryDependencies ++= Seq(
  "org.scalaz"                  % "scalaz-core_2.9.1"   % "6.0.2",
  "com.typesafe.akka"           % "akka-actor"          % "2.0",
  "com.weiglewilczek.slf4s"     % "slf4s_2.9.1"         % "1.0.7",
  "org.specs2"                  % "specs2_2.9.1"        % "1.8"            % "test",
  "org.mockito"                 % "mockito-all"         % "1.9.0"          % "test",
  "org.scala-tools.testing"     % "scalacheck_2.9.1"    % "1.9"            % "test"
)
