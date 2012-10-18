# NOTE

BlueEyes 1.0 development is currently underway in the master branch. If you wish to use the current stable version of BlueEyes, please use the 0.6x branch.

While many features will be retained, BlueEyes 1.0 is not expected to be backward compatible -- some reworking will be required.

# BlueEyes

BlueEyes is a lightweight, asynchronous web framework for the Scala programming language. The framework lets you quickly and easily create high-performing web services that embrace the machinery and language of HTTP. The framework tries to get out of your way and let you concentrate on logic instead of boilerplate.

BlueEyes has been used in production across large clusters of instances deployed in Amazon EC2, reliably handling tens of thousands of requests a second, in an environment with 24x7 uptime requirements (online display advertising).

The framework has been designed to meet the following requirements:

  * Stateless design, to achieve massive scalability;
  * Purely asynchronous request handling, to achieve extremely fast per-instance performance;
  * Highly composable, modular design that minimizes bloat and surface area of the API;
  * Declarative service construction;
  * Support for continuous deployment and automated testing;
  * Idiomatic Scala interfaces to highly-scalable databases such as MongoDB.

BlueEyes does not have any features for server-side generation of HTML, CSS, or JavaScript. BlueEyes does not (natively) serve static files, like Apache or Jetty. BlueEyes is intended *only* for creating RESTful web services that are consumed by clients (such as browsers or servers).

Those looking for a traditional model/view web framework for the Scala programming language are directed to the [Lift Web Framework](http://www.liftweb.net/).

## Mailing List

If you have bugs to report, please use the GitHub issues tracker. If you have questions about BlueEyes, you are invited to join the BlueEyes Web Framework discussion group:

  * [BlueEyes Web Framework Discussion Group](http://groups.yahoo.com/group/blueeyes-web)

## Book (in-progress)

For more extensive documentation on BlueEyes, see the [in-progress book](http://noelwelsh.com/blueeyes/index.html)

## Maven

Repositories:

 * http://nexus.scala-tools.org/content/repositories/
 * http://scala-tools.org/repo-snapshots/
 * http://repository.jboss.org/nexus/content/groups/public/
 * http://repo.akka.io/releases/
 * http://guiceyfruit.googlecode.com/svn/repo/release/

Library dependency:

```xml
<dependency>
  <groupId>com.reportgrid</groupId>
  <artifactId>blueeyes</artifactId>
  <version>0.4.24</version>
  <type>jar</type>
  <scope>compile</scope>
</dependency>
```

### SBT 0.10+

```scala
resolvers ++= Seq(
  "Sonatype"    at "http://nexus.scala-tools.org/content/repositories/public",
  "Scala Tools" at "http://scala-tools.org/repo-snapshots/",
  "JBoss"       at "http://repository.jboss.org/nexus/content/groups/public/",
  "Akka"        at "http://repo.akka.io/releases/",
  "GuiceyFruit" at "http://guiceyfruit.googlecode.com/svn/repo/releases/"
)

libraryDependencies ++= Seq(
  "com.reportgrid" % "blueeyes_2.9.2" % "0.6" % "compile"
)
```

## Origins

BlueEyes is loosely inspired by the Ruby library *Sinatra* and the Scala library *Scalatra*. These lightweight libraries allow developers to easily create RESTful services without the feature bloat and poor usability common to most web frameworks.

BlueEyes aims for the same or higher level of productivity as these libraries, but with a more functional design, much higher performance, and compatibility with the rigorous demands of continuous deployment.

## Team

<table>
  <thead>
    <tr>
      <td>Name</td>               <td>Role</td>                                                                    <td>Twitter</td>
    </tr>
  </thead>
  <tbody>
    <tr>
      <td>John A. De Goes</td>    <td>Author &amp; architect, core platform</td>                                    <td><a href="http://twitter.com/jdegoes">@jdegoes</a></td>
    </tr>
    <tr>
      <td>Kris Nuttycome</td>    <td>Core platform</td>                                                             <td><a href="http://twitter.com/nuttycom">@nuttycom</a></td>
    </tr>
    <tr>
      <td>Michael Lagutko</td>    <td>Core platform, persistence</td>                                               <td><a href="http://twitter.com/mlagutko">@mlagutko</a></td>
    </tr>
    <tr>
      <td>Jeff Simpson</td>       <td>Asynchronous HTTP client</td>                                                 <td><a href="http://twitter.com/fooblahblah">@fooblahblah</a></td>
    </tr>
    <tr>
      <td>Noel Welsh</td>    <td>General fixes, documentation, community building</td>         <td><a href="http://twitter.com/noelwelsh">@noelwelsh</a></td>

    </tr>
</tbody>
</table>

## Development

To release

- Login at oss.sonatype.org
- Run the publish command for core, json, and mongo

## License

Copyright (c) 2010-2012

Published under The MIT License

## Sponsors

A big round of thanks to the sponsors of BlueEyes.

<a href="http://www.ej-technologies.com/products/jprofiler/overview.html">JProfiler</a> - Best-in-class profiler for Java and Scala developers
