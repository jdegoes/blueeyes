resolvers ++= Seq("sbt-idea-repo" at "http://mpeltonen.github.com/maven/")

resolvers += Resolver.url("sbt-plugin-releases", new URL("http://scalasbt.artifactoryonline.com/scalasbt/sbt-plugin-releases/"))(Resolver.ivyStylePatterns)

addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "0.7.2")

addSbtPlugin("com.github.mpeltonen" % "sbt-idea" % "0.11.0")

addSbtPlugin("com.jsuereth" % "xsbt-gpg-plugin" % "0.6")

