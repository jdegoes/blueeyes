name := "blueeyes-mongo"

fork := true

libraryDependencies ++= Seq(
  "org.mongodb"                 % "mongo-java-driver"   % "2.7.3",
  "rhino"                       % "js"                  % "1.7R2",
  "org.specs2"                  %% "specs2"             % "1.8"            % "test",
  "org.mockito"                 % "mockito-all"         % "1.8.5"          % "test",
  "org.scala-tools.testing"     %% "scalacheck"         % "1.9"            % "test"
)

parallelExecution in Test := false
