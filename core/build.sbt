name := "blueeyes-core"

libraryDependencies ++= Seq(
  "com.googlecode.concurrentlinkedhashmap" % "concurrentlinkedhashmap-lru" % "1.1",
  "com.typesafe.akka"           % "akka-actor"          % "2.0.2",
  "com.weiglewilczek.slf4s"     % "slf4s_2.9.1"         % "1.0.7",
  "commons-codec"               % "commons-codec"       % "1.5",
  "javolution"                  % "javolution"          % "5.5.1",
  "javax.servlet"               % "javax.servlet-api"   % "3.0.1" % "provided",
  "joda-time"                   % "joda-time"           % "1.6.2",
  "org.jboss.netty"             % "netty"               % "3.2.6.Final",
  "org.streum"                  %%  "configrity-core"   % "0.10.2",
  "org.xlightweb"               % "xlightweb"           % "2.13.2",
  "org.mockito"                 %  "mockito-all"        % "1.9.0"          % "test",
  "org.eclipse.jetty"           % "jetty-server"        % "8.1.3.v20120416"          % "test",
  "org.eclipse.jetty"           % "jetty-servlet"       % "8.1.3.v20120416"          % "test"
)

ivyXML := 
<dependency org="org.eclipse.jetty.orbit" name="javax.servlet" rev="3.0.0.v201112011016" conf="test">
<artifact name="javax.servlet" type="orbit" ext="jar"/>
</dependency>

parallelExecution in Test := false


