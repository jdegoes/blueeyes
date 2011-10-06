# BlueEyes

BlueEyes is a lightweight web 3.0 framework for the Scala programming language. The framework lets you quickly and easily create high-performing web services that embrace the machinery and language of HTTP. The framework tries to get out of your way and let you concentrate on logic instead of boilerplate.

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

## Maven

Repositories:

 * http://nexus.scala-tools.org/content/repositories/
 * http://scala-tools.org/repo-snapshots/
 * http://repository.jboss.org/nexus/content/groups/public/
 * http://akka.io/repository/
 * http://guiceyfruit.googlecode.com/svn/repo/release/

Library dependency:

```xml
<dependency>
  <groupId>com.reportgrid</groupId>
  <artifactId>blueeyes</artifactId>
  <version>0.4.26</version>
  <type>jar</type>
  <scope>compile</scope>
</dependency>
```

### SBT before 0.10

```scala
val sonatype_repo     = MavenRepository("Sonatype",     "http://nexus.scala-tools.org/content/repositories/")
val scala_tools_repo  = MavenRepository("Scala Tools",  "http://scala-tools.org/repo-snapshots/")
val jboss_repo        = MavenRepository("JBoss",        "http://repository.jboss.org/nexus/content/groups/public/")
val akka_repo         = MavenRepository("Akka",         "http://akka.io/repository/")
val guicey_fruit_repo = MavenRepository("GuiceyFruit",  "http://guiceyfruit.googlecode.com/svn/repo/release/")

val blueeyesRelease = "com.reportgrid" % "blueeyes" % "0.4.26" % "compile"
```

### SBT 0.10

```scala
resolvers ++= Seq(
  "Sonatype"    at "http://nexus.scala-tools.org/content/repositories/public",
  "Scala Tools" at "http://scala-tools.org/repo-snapshots/",
  "JBoss"       at "http://repository.jboss.org/nexus/content/groups/public/",
  "Akka"        at "http://akka.io/repository/",
  "GuiceyFruit" at "http://guiceyfruit.googlecode.com/svn/repo/releases/"
)

libraryDependencies ++= Seq(
  "com.reportgrid" % "blueeyes_2.9.1" % "0.4.26" % "compile"
)
```

## Origins

BlueEyes is loosely inspired by the Ruby library *Sinatra* and the Scala library *Scalatra*. These lightweight libraries allow developers to easily create RESTful services without the feature bloat and poor usability common to most web frameworks.

BlueEyes aims for the same or higher level of productivity as these libraries, but with a more functional design, much higher performance, and compatibility with the rigorous demands of continuous deployment.

## Services

The fundamental concept in BlueEyes is the *service*. A *service* responds to requests. Every service is uniquely identified by a name and a version.

A service goes through three distinct phases in its lifecycle:

  1. *Startup*. The service performs any setup operations required to perform its duties, such as loading data.
  2. *Request*. The service responds to requests.
  3. *Shutdown*. The service performs any cleanup operations, such as disposing of resources.

In the request phase, services handle different HTTP verbs (GET, POST, PUT, DELETE) on different paths, accepting and producing different mime types.

Services are generally built using *BlueEyesServiceBuilder*, which allows easy, declarative service construction.

The following code builds an e-mail service, together with a server capable of running the service from the command-line:

```scala
trait EmailServices extends BlueEyesServiceBuilder {
  val emailService = service("email", "1.32") { context =>
    startup {
      loadContactList(context.config("contactFile"))
    } ->
    request { contactList =>
      path("/emails/") {
        contentType(application/json) {
          get { request =>
            ...
            Future.sync(HttpResponse(content = Some(JArray(emailIds))))
          } ~
          path('emailId) {
            get { request =>
              val emailId = request.parameters('emailId)
              ...
              Future.sync(HttpResponse(content = Some(emailObj)))
            }
          }
        }
      }
    } ->
    shutdown { contactList =>
      contactList.finalize
    }
  }
}
object EmailServer extends BlueEyesServer with EmailServices
```

Services are automatically provided with *context*, which provides a bundle of functionality essential to every service:

 * *config*. Every service gets its own separate config block, namespaced by service name and major version (*services.[serviceName].v[serviceMajorVersion]*)
 * *serviceName*. Name of the service.
 * *serviceVersion*. Version of the service.

The sections that follow explore different aspects of dealing with services.

### Construction

Fundamentally, a service is a request handler -- that is, it processes incoming HTTP requests, and responds to them with HTTP responses.

In BlueEyes, a request handler is a *partial function from request to a future of response*. Formally:

```scala
type HttpRequestHandler2[T, S] = PartialFunction[HttpRequest[T], Future[HttpResponse[S]]]
```

Since a request handler is just an ordinary partial function, it's possible to construct one in many ways:

```scala
new PartialFunction[HttpRequest[T], Future[HttpResponse[T]]] {
  def isDefinedAt(request: HttpRequest[T]): Boolean = ...

  def apply(request: HttpRequest[T]): Future[HttpResponse[T]] = ...
}

{
  case HttpRequest(...) => ...
  case HttpRequest(...) => ...
  case HttpRequest(...) => ...
}
```

However, these approaches to constructing partial functions are tedious, do not compose, and are difficult to read. BlueEyes ships with *request handler combinators* that enable declarative construction of request handlers. A few of the more common combinators are listed below:

 * path(*pattern*) { ... }
 * get { ... }
 * put { ... }
 * post { ... }
 * delete { ... }
 * contentType(*mimeType*) { ... }
 * accepts(*mimeType*) { ... }
 * produces(*mimeType*) { ... }
 * parameter(*parameterId*) { parameterValue => ... }

These combinators can be combined in the obvious ways:

```scala
path("/users/'userId") {
  produces(application/json) {
    get { request =>
      // get user
      val userId = request.parameters('userId)
      ...
    }
  }
}
```

The only limitation is that the *get*/*put*/*post*/*delete* combinators must be innermost expressions. These combinators accept *full* functions (not partial functions).

As partial functions, request handlers can be combined through the standard Scala *orElse* method, which will delegate to the first handler that is defined for a specified request:

```scala
path("/food") {
  ...
}.orElse {
  path("/foo/'fooId") {
    ...
  }
}
````

BlueEyes provides the join operator '~' as an alternative to *orElse*. Using the join operator can make your request handlers easier to read:

```scala
produces(application/json) {
  path("/users/") {
    get { request =>
      // get list of all users
      ...
    } ~
    path('userId) {
      parameter('userId) { userId =>
        get { request =>
          // get user
          ...
        } ~
        put { request =>
          // update user
          ...
        }
      }
    }
  }
}
```

An entire category of combinators is devoted to extracting data from requests in order to reduce duplication (in the above snippet, the *parameter* combinator is used to avoid duplicate extraction of the user ID). The next section explains these combinators in greater depth.

#### Extractor Combinators

#### Path Combinator

The path combinator deserves special treatment. Although in the preceding examples, we passed strings to the path combinator, the function actually accepts a *RestPathPattern*, which can be implicitly created from a string.

Rest path patterns are composed from the following building blocks:

 * String literals, such as "/foo/bar".
 * Symbols, which are placeholders for url fragments, such as "/foo/'fooId". Symbols do not match path separator characters or periods, although they do match underscores, dashes, numbers, and spaces. Symbols are extracted and placed into request parameters, keyed by symbol name.
 * Regular expressions, such as "/foo/bar/baz.(?<extension>\w{3,4})". Any part of a string wrapped in parentheses is automatically treated as a regular expression. Named capture groups are extracted and placed into request parameters, keyed by capture group name.

Although it's most common to create patterns from strings or symbols, you can also create them using the methods available on an existing pattern. For example:

```scala
pattern / 'foo
pattern / "foo"
pattern / "(?<email>\w+@\w+\.\w{2,3})"
```

### Consumption

The goal of providing HTTP services is consuming them. BlueEyes includes a high-performance, asynchronous HTTP client that is unified with the rest of the BlueEyes stack.

The core client interface is *HttpClient*, which is a partial function from request to a future of response. The *apply* method is seldom used directly. Instead, BlueEyes provides methods for all http request, such as "GET", "POST", etc.

Given a reference to *client*, you could perform a simple HTTP GET on the path "/foo" with the following code:

```scala
val responseFuture = client.get("http://myservice.com/foo")
responseFuture map {response => response.content.get}
```

If you're going to perform a lot of requests that all share the same or similar structure, then you can create your own client:

```scala
def myService: HttpClient[JValue] = client.path("http://myservice.com/").contentType[JValue](application/json)

val responseFuture = myService.get("api/v1")
responseFuture map {response => response.content.get}
```

Contrary to these toy examples, in real world usage, you would not simply get the content of the response. Rather, you'd extract out whatever information you need and transform it into the desired value.

### Testing

BlueEyes is built from the ground up to support automated, comprehensive, fast-running tests.

The testing framework is currently compatible with *Specs*, and extends the *Specification* trait to make testing services easy.

To test your services with *Specs*, you should extend *BlueEyesServiceSpecification* with whatever services you want to test. This trait, in turn, mixes in a helper "service" method to create service client.

```scala
class EmailServicesSpec extends BlueEyesServiceSpecification with EmailServices {
  "EmailService" should {
    "get emails" in {
      val f = service.contentType[JValue](application/json).get("/emails")
      f.value must eventually(beSomething)

      val response = f.value.get
      response.status mustEqual(HttpStatus(OK))
    }
  }
}
```

These combinators produce very descriptive *Specs* messages, because they are fully aware of the path, HTTP method, and query string parameters you are using to invoke the service. This eliminates duplication between textual description and test logic, and makes you more productive.

If a service uses mongo facade then it is convinient to use factory trait ConfigurableMongo to create a Mongo facade. The factor creates either RealMongo or MockMongo deppending on "mongo.mock" JVM parameter. If the JVM parameter is set to "true" then MockMongo is created otherwise RealMongo is created. Using "mongo.mock" switch allows easy change Mock Mongo and Real Mongo in tests. If the JVM parameter "mongo.moc" is not set then BlueEyesServiceSpecification sets the parameter
to "true" and MockMongo is used in tests. Factory "mongo" method takes a configuration as a parameter with mongo server configuration. If the configuration contains section dropBeforeStart then all specified collection(s) on specified database(s) are dropped before starting.
Sample configuration is:

```scala
dropBeforeStart {
  mydb = ["mycollection"]
}
```

### Execution

Services are run through a *server*. A "server" in this context refers to a *process*, not a *machine* -- any number of servers can run on the same physical machine.

To create a server, BlueEyes includes the *BlueEyesServer* trait, which is typically extended by an *object*. You can specify all the services you want the server to run just by mixing in the traits that build them. For example, the following code creates a server that runs four services:

```scala
object AppServer extends BlueEyesServer with EmailServices with OrderProcessingServices with LoginServices with CatalogServices
```

A server created in this way has *start* and *stop* methods, which can be used for starting and stopping the services. The server also defines a *main* method that accepts a *--configFile* command-line option to indicate which configuration file should be used to configure the server and all the services.

    java -jar appserver.jar --configFile /etc/default/appserver.conf

A single server can run any number of services, although the recommended practice is to run each service on a separate server, on a separate port, and use a load balancer like *HAProxy* to unify the HTTP interface to the services. This approach confers a number of benefits:

 * Independent provisioning of services based on requirements (some services may be needed to maintain 100% uptime and thus may be replicated across instances and data centers, while others may not need such high-availability);
 * Independent scaling of services based on load;
 * Isolation of services so that the crash of one service has no effect on others;
 * Independent deployment of services so that risk to production is minimized.

#### Server Configuration Options

Server Configuration included the following options: 

- port: the port the server is started on, which defaults to 8888".
- sslPort: the port the ssl server is started on, which defaults to 8889.
- address:  the local addres the server will bind to, which defaults to "localhost". 
- sslEnable: sets if the ssl should be running, which defaults to true.
- chunkSize: the chunk size in bytes of a request/response content, which defaults to 1048576. 

The minimal configuration looks like:

```scala
server {
  port = 8585
  sslPort = 8586
}
```

### Augmentation

Services can be augmented in a variety of ways -- for example, with loggers, health monitors, and service locators. The augmentation facility is based on composition of so-called *service descriptor factories*, which are functions that accept a service context and return a service descriptor.

A service descriptor factory is at the heart of every service declaration. For example, take the following minimal service declaration:

```scala
val myService = service("myservice", "2.39.23") { context =>
  request { state =>
    path("/foo") {
      contentType(application/json) {
        get { request =>
          ...
        }
      }
    }
  }
}
```

The anonymous final argument passed to the *service* function is actually a service descriptor factory. That is, the argument is a function takes the context of the service, and returns a service descriptor, which describes the service lifecycle (startup, request handling, and shutdown).

BlueEyes ships with many useful service descriptor factory combinators that are designed to augment your service with additional features. For example, the *logging* combinator adds logging to your service:

```scala
val myService = service("myservice", "2.39.23") {
  logging { logger =>
    context => 
      request { state =>
        path("/foo") {
          contentType(application/json) {
            get { request =>
              ...
            }
          }
        }
      }
  }
}
```

The sections that follow describe the most common ways to augment your services.

#### Logging

BlueEyes provides a combinator that provides services with a logger that can be configured independently for each service.

```scala
trait LogDemo extends BlueEyesServiceBuilder {
  val logDemoService = service("logdemo", "1.32") {
    logging { log =>
      context =>
        startup {
          request { state =>
            path("/foo") {
              contentType(application/json) {
                get { request =>
                  log.info("request at /foo")
                  ...
                }
              }
            }
          }
        }
    }
  }
}
```

A service's logger is configured through a *log* block inside the root config for the service.

#### Request Logging
Request Logging allows services to log requests/responses in W3C Extended Log format (http://www.w3.org/TR/WD-logfile.html).

```scala
trait RequestLogDemo extends BlueEyesServiceBuilder {
  val requestLogDemoService = service("requestlogdemo", "1.32") {
    requestLogging {
      context =>
        startup {
          request { state =>
            path("/foo") {
              contentType(application/json) {
                get { request =>
                  ...
                }
              }
            }
          }
        }
    }
  }
}
```

A service's request logger is configured through a *requestLog* block inside the root config for the service.

The following values can be configured for request logging:

   enabled           = true | false ( default = true )
   fields            = "W3C Extended Log format fields", "sc-content", "sc-content" 
   cs-content        = request content encoded by Base64 encoding and wrapped by double quotes
   sc-content        = response content encoded by Base64 encoding and wrapped by double quotes
   roll              = "never" | "hourly" | "daily" | "sunday" | "monday" | "tuesday" | "wednesday" | "thursday" | "friday" | "saturday" " ( default = "never")
   file              = path to log file
   includePaths      = list of paths (regualar expressions) for which requests/responses must be logged
   excludePaths      = list of paths (regualar expressions) for which requests/responses must not be logged   
   writeDelaySeconds = delay between flush to file ( default = 1 )
   
   If neither "includePaths" nor "excludePaths" are specified then all requests/responses are logged. 
   If both "includePaths" and "excludePaths" are specified then only "includePaths" are taken into account.  

```scala
services {
  requestlogdemo {
    v1 {
      requestLog {
        fields            = "cs-method cs-uri cs-content sc-content"
        roll              = "never"
        file              = "./logs"
        includePaths      = ["/foo/.*", "/bar/.*"]
        writeDelaySeconds = 5
      }
    }
  }
}
```

#### Health Monitor

Health monitor allows services to export real-time metrics on health status, for use in continuous deployment.

The default health monitor automatically exports information on number of requests, number and type of errors, and length of requests.

```scala
trait HealthMonitorDemo extends BlueEyesServiceBuilder {
  val healthMonitorService = service("healthmon", "1.32") {
    healthMonitor { monitor =>
      context =>
        request { state =>
          path("/foo") {
            contentType(application/json) {
              get { request =>
                monitor.time(".requests.foo.timing") {
                  ...
                }
              }
            }
          }
        }
    }
  }
}
```

Health metrics are exported in JSON form through an HTTP GET. For a particular service, the health can be queried at the following URL:

 * /blueeyes/services/[serviceName]/v[serviceMajorVersion]/health

For example:  */blueeyes/services/healthmon/v1/health*

#### Service Locator

If you have multiple services, and one service needs to consume another, you can use the service locator combinator. This combinator uses information in a config file to determine where to locate services, and provides a tailor-made client that can be used to communicate with them.

```scala
trait ServiceLocatorDemo extends BlueEyesServiceBuilder {
  val serviceLocatorService = service ("email", "1.01") {
    serviceLocator { locator =>
      context => {
        request {
          path("/foo") {
            get { request: HttpRequest[String] =>
              // Locate foo/v1 service and perform HTTP GET on /bar path
              val content = locator("foo", "1.02.32") { client =>
                client.get("/bar").map(response => response.content)
              }

              // Do something with content
              ...
            }
          }
        }
      }
    }
  }
}
```

#### Configurable Root

The configurable root combinator uses the *rootPath* setting in the service's config block to shift the request handler rightward by the specified string. This is useful when you want to combine many services on a single server, and avoid name clashes.

```scala
trait ConfigurableRootDemo extends BlueEyesServiceBuilder {
  val configurableRootService = service("configurableroot", "1.0.2") {
    configurableRoot {
      request {
        path("/foo") { // true path will be context.config("services.configurableroot.v1.rootPath") + "/foo"
          ...
        }
      }
    }
  }
}
```

## Data Exchange

### JSON

BlueEyes comes with the most fully-featured Scala library for JSON parsing, rendering, and manipulation. The library is
derived from Lift Json, but with more features and a more uniform API.

## Persistence

### Cache

BlueEyes has several kinds of caches:

 * Concurrent cache, which can be used by any number of threads without synchronizing;
 * Stage, most often used to coalesce IO operations in a staging area before execution.

All caches support user-defined expiration policy and eviction handlers.

### MongoDB

BlueEyes has a full-featured Scala facade to MongoDB.

First of all, you need to create an instance to the Mongo facade. You have your choice of RealMongo or MockMongo. The latter is a memory-only Mongo facade that is designed for automated testing.

Factory trait ConfigurableMongo can be used to create a Mongo facade. The factor creates either RealMongo or MockMongo deppending on "mongo.mock" JVM parameter. If the parameter is set to "true"
then MockMongo is created otherwise RealMongo is created. Factory "mongo" method takes a configuration as a parameter with mongo server configuration. If the configuration contains section 
dropBeforeStart then all specified collection(s) on specified database(s) are dropped before starting.
Sample configuration is:

```scala
dropBeforeStart {
  mydb = ["mycollection"]
}
```

Once you have access to Mongo, you can then create references to databases:

```scala
val database = mongo.database( "mydb" )
```

To modify or retrieve documents from a database, you first create a query and then execute it using the database instance (created in the previous example).

```scala
val query    = selectOne().from("mycollection").sortBy("foo.bar" <<)
val document = database(query)
```

To restrict the scope of a query, you need to create a filter. Possible filters include: "===" (equal), "!==" (not equal), ">" (greater), "<" (less), ">=" (greater or equal), "<=" (less or equal), "anyOf" (possible matches), "contains" (all possible matches), "hasSize" (array with the specified number of elements), "exists" (field existence), "hasType" (values matches), "regex" (regular expressions).

  * Equals filter:
      "foo" === "bar"

  * Greater filter:
      "foo" > "bar"

It is possible to combine queries in more complex complex query using "|" (or) and "&" (and) operators.
    "foo" === "bar" | "foo" > "baz"

BlueEyes supports documents manipulations queries: querying, removing, updating (whole document and some documents fields) and inserting, index queries, group/map reduce queries:

1. Querying documents queries

  * Select all documents:
    val query    = select().from("mycollection")

  * Select multiple documents and sort by a field:   
    val query    = select().from("mycollection").sortBy("foo.bar" <<)

  * Select multiple documents by criteria:
    val query    = select().from("mycollection").where("foo.bar" === "blahblah")

  * Select multiple documents and skip some documents:
    val query    = select().from("mycollection").skip(20)

  * Select limited count of documents:
    val query    = select().from("mycollection").limit(20)

  * Select only some documents fields:
    val query    = select("foo", "bar").from("mycollection")

  * Distinct multiple documents. It is possible to sort documents, select by criteria, skip documents, select limited count of documents and select only some documents fields (see examples above):
    val query    = distinct().from("mycollection")

  * Select one document. It is possible to sort documents and select by criteria (see examples above):
    val query    = selectOne().from("mycollection")

  * Select documents count in collection:
    val query    = count.from("mycollection")

  * Select documents count by criteria:
    val query    = count.from("mycollection")

2. Removing documents queries

 * Remove all documents:
    val query    = remove.from("mycollection")

 * Remove documents by criteria:
    val query    = remove.from("mycollection").where("foo.bar" === "blahblah")

3. Inserting documents queries

  * Insert documents:
    val query    = insert(jObject1, jObject2).into("mycollection")

4. Index modification queries

  * Ensure index exists:
    val query    = ensureIndex("index1").on("mycollection", "foo", "bar")

  * Ensure unique index exists:
    ensureUniqueIndex("index1").on("mycollection", "foo", "bar")

  * Drop index:
    val query    = dropIndex("index1").on("mycollection")

  * Drop indexes:
    val query    = dropIndexes.on("mycollection")

5. Updating documents queries (It is possible to update both and some documents fields)

  * Update one document:
    val query    = update("mycollection").set(MongoUpdateObject(jObject))

  * Update one document by criteria:
    update("mycollection").set(MongoUpdateObject(jObject)).where("foo.bar" === "blahblah")

  * Update many documents:
    val query    = updateMany("mycollection").set(MongoUpdateObject(jObject)).where("foo.bar" === "blahblah")

  * Upsert one document:
    val query    = upsert("mycollection").set(MongoUpdateObject(jObject)).where("foo.bar" === "blahblah")

  * Upsert many documents:
    val query    = upsertMany("mycollection").set(MongoUpdateObject(jObject)).where("foo.bar" === "blahblah")

6. Group/map reduce queries

  * Group query
    val intial = JsonParser.parse("""{ "csum": 10.0 }""").asInstanceOf[JObject]
    val query  = group(initial, "function(obj,prev) { prev.csum += obj.bar}", "foo").from("mycollection")

  * Map reduce query
    val map     = """function(){ this.tags.forEach( function(z){ emit( z , { count : 1 } ); } );};"""
    val reduce  = """function( key , values ){ var total = 0; for ( var i=0; i<values.length; i++ ) total += values[i].count; return { count : total }; };"""
    val query   = map(map, reduce).from("mycollection")

To know whether or not operation succeeded, or if it did not succeed, what error it generated it is necessary to create "verified" query:
    val query   =  verified(selectOne().from("mycollection").where("foo.bar" === "blahblah").sortBy("foo.bar" <<))

To update document field it is necessary to create Mongo Update. Possible updates: "inc" (increments field), "set" (sets field), "unset" (unset fields), "popLast" (removes the last element in an array), "popFirst" (removes the first element in an array), "push" (appends value to array field), "pull" (removes all occurrences of value from array field), "pushAll" (appends each value to array field), "pullAll" (removes all occurrences of each value from array field).
 * Mongo update
    val query   = "foo" unset

It is possible to combine simple updates into complex update using & (and) operator"
    val query   = "foo" unset & "bar" set (1)

## Continuous Deployment

BlueEyes is designed to support the lean development practice of _continuous deployment_. In continuous deployment, the team can safely deploy code as often as required by business needs -- even many times a day.

In order for a team to practice continuous deployment successfully, two critical requirements must be met:

 1. The entire code base needs to have comprehensive, fast automated tests, typically at the unit level (integration and system tests can be included only if they run quickly).
 2. The application needs to export real-time health metrics for use in detecting quality issues post-deployment (for the most part, these metrics supplant integration and system tests)

On top of this foundation, a continuous deployment system can be built, which does incremental deployments with health checks, and which either backs out defective releases automatically, or makes it easy for team members to do so.

BlueEyes provides support for both pillars:

 1. BlueEyes makes testing web services extremely easy, and the tests do not start a real server so they run very quickly.
 2. BlueEyes makes it easy to export real-time health metrics (and, if you use health monitor, automatically exports all the critical metrics).

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
      <td>Jeff Simpson</td>       <td>Asynchronous HTTP client</td>                                                 <td></td>
    </tr>
    <tr>
      <td>Mike Conigliaro</td>    <td>Container/Deployment Manager for BlueEyes Services (unreleased)</td>         <td><a href="http://twitter.com/mconigliaro">@mconigliaro</a></td>
    </tr>
  </tbody>
</table>

## License

Copyright (c) 2010-2011

Published under The MIT License

## Sponsors

A big round of thanks to the sponsors of BlueEyes. 

<a href="http://www.ej-technologies.com/products/jprofiler/overview.html">JProfiler</a> - Best-in-class profiler for Java and Scala developers
