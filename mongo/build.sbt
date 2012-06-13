name := "blueeyes-mongo"

fork := true

libraryDependencies ++= Seq(
  "org.mongodb"                 % "mongo-java-driver"   % "2.7.3",
  "rhino"                       % "js"                  % "1.7R2",
  "org.specs2"                  % "specs2_2.9.1"        % "1.10"           % "test",
  "org.mockito"                 % "mockito-all"         % "1.9.0"          % "test",
  "org.scala-tools.testing"     % "scalacheck_2.9.1"    % "1.9"            % "test"
)

parallelExecution in Test := false
