resolvers ++= Seq("Condep repo" at "http://samskivert.github.com/sbt-condep-plugin/maven",
                 "sbt-idea-repo" at "http://mpeltonen.github.com/maven/")

addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "0.7.2")

addSbtPlugin("com.samskivert" %% "sbt-condep-plugin" % "1.1")

addSbtPlugin("com.github.mpeltonen" % "sbt-idea" % "0.11.0")
