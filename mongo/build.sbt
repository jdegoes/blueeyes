name := "blueeyes-mongo"

fork := true

libraryDependencies ++= Seq(
  "org.mongodb"                 % "mongo-java-driver"   % "2.7.3",
  "rhino"                       % "js"                  % "1.7R2",
  "org.mockito"                 % "mockito-all"         % "1.9.0"          % "test"
)

parallelExecution in Test := false
