name := "blueeyes-json"

publishArtifact in Test := true

libraryDependencies ++= Seq(
  "org.scalaz"                  %% "scalaz-core"        % "7.0-SNAPSHOT" changing(),
  "joda-time"                   % "joda-time"           % "1.6.2"          % "optional",
  "org.specs2"                  %% "specs2"             % "1.10"           % "test",
  "org.scala-tools.testing"     %% "scalacheck"         % "1.9"            % "test"
)
