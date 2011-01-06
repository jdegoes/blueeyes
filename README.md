# BlueEyes

BlueEyes is a lightweight web 3.0 framework for the Scala programming language. The framework is designed to allow developers to quickly and easily create high-performing web services that embrace the machinery and language of HTTP.

BlueEyes has been used in production across large clusters of instances deployed in Amazon EC2, reliably handling tens of thousands of requests a second.

The framework has been designed to meet the following requirements:

  * Stateless design, to achieve massive scalability;
  * Purely asynchronous request handling, to achieve extremely fast per-instance performance;
  * Highly composable, modular design that minimizes bloat and surface area;
  * Declarative service construction;
  * Support for continuous deployment and automated testing;
  * Idiomatic Scala interfaces to highly-scalable databases such as MongoDB.

BlueEyes does not have any features for server-side generation of HTML, CSS, or JavaScript. BlueEyes does not (natively) serve static files, like Apache or Jetty. And BlueEyes will *never* have any support for forms, AJAX, widgets, and other client-side components.

BlueEyes is intended *only* for creating RESTful web services.

Those looking for a traditional MVC web framework for the Scala programming language are directed to the [Lift Web Framework](http://www.liftweb.net/).

## Maven

Repository: http://oss.sonatype.org/content/repositories/releases

    <dependency>
      <groupId>com.github.blueeyes</groupId>
      <artifactId>blueeyes</artifactId>
      <version>0.1.36</version>
      <type>jar</type>
      <scope>compile</scope>
    </dependency>

### SBT

    val sonatypeRepository = MavenRepository("Sonatype Releases", "http://oss.sonatype.org/content/repositories/releases")
    
    val blueeyesRelease = "com.github.blueeyes" % "blueeyes" % "0.1.36" % "compile"

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
                  HttpResponse(content = Some(JArray(emailIds)))
                } ~
                post { request =>
                  ...
                  HttpResponse(status = OK)
                } ~
                path('emailId) {
                  get { request =>
                    val emailId = request.parameters('emailId)
                    ...
                    HttpResponse(content = Some(emailObj))
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

Services are automatically provided with *context*, which provides a bundle of functionality essential to every service:

 * *config*. Every service gets its own separate config block, namespaced by service name and major version (*services.[serviceName].v[serviceMajorVersion]*)
 * *serviceName*. Name of the service.
 * *serviceVersion*. Version of the service.

The sections that follow explore different aspects of dealing with services.

### Construction

Fundamentally, a service is a request handler -- that is, it processes incoming HTTP requests, and responds to them with HTTP responses.

In BlueEyes, a request handler is a *partial function from request to a future of response*. Formally:

    type HttpRequestHandler2[T, S] = PartialFunction[HttpRequest[T], Future[HttpResponse[S]]]

Since a request handler is just an ordinary partial function, it's possible to construct one in many ways:

    new PartialFunction[HttpRequest[T], Future[HttpResponse[T]]] {
      def isDefinedAt(request: HttpRequest[T]): Boolean = ...
      
      def apply(request: HttpRequest[T]): Future[HttpResponse[T]] = ...
    }
    
    {
      case HttpRequest(...) => ...
      case HttpRequest(...) => ...
      case HttpRequest(...) => ...
    }

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

    path("/users/'userId") {
      produces(application/json) {
        get { request =>
          // get user
          val userId = request.parameters('userId)
          ...
        }
      }
    }

The only limitation is that the *get*/*put*/*post*/*delete* combinators must be innermost expressions. These combinators accept *full* functions (not partial functions).

As partial functions, request handlers can be combined through the standard Scala *orElse* method, which will delegate to the first handler that is defined for a specified request:

    path("/food") {
      ...
    }.orElse {
      path("/foo/'fooId") {
        ...
      }
    }

BlueEyes provides the join operator '~' as an alternative to *orElse*. Using the join operator can make your request handlers easier to read:

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

An entire category of combinators is devoted to extracting data from requests in order to reduce duplication (in the above snippet, the *parameter* combinator is used to avoid duplicate extraction of the user ID). The next section explains these combinators in greater depth.

#### Extractor Combinators

#### Path Combinator

The path combinator deserves special treatment. Although in the preceding examples, we passed strings to the path combinator, the function actually accepts a *RestPathPattern*, which can be implicitly created from a string.

Rest path patterns are composed from the following building blocks:

 * String literals, such as "/foo/bar".
 * Symbols, which are placeholders for url fragments, such as "/foo/'fooId". Symbols do not match path separator characters or periods, although they do match underscores, dashes, numbers, and spaces.
 * Regular expressions.

Although it's most common to create patterns from strings or symbols, you can also create them using the methods available on an existing pattern. For example:

    pattern / 'foo
    pattern / "foo"

### Consumption

The dual of providing HTTP services is consuming them. BlueEyes includes a high-performance, asynchronous HTTP client that is unified with the rest of the BlueEyes stack.

The core client interface is *HttpClient*, which is a partial function from request to a future of response. The *apply* method is seldom used directly. Instead, BlueEyes provides so-called *client transformer combinators*, which are functions that map an *HttpClient* into a future of some value.

For the most part, the client transformer combinators mirror the request handler combinators, except all methods are suffixed with a dollar sign character ('$'). This means you can consume services using a syntax almost identical to the one you use to construct them.

Given a reference to *client*, you could perform a simple HTTP GET on the path "/foo" with the following code:

    val content = client {
      path$("http://myservice.com/foo") {
        get$ { response =>
          response.content.get
        }
      }
    }

The great power of this design lies in its composability. If you wanted to perform an HTTP GET on path "/foo/bar" at the same time as the first GET, without duplicating all the same code, you could do so using the join operator ('~'):

    val contentTuple = client {
      path$("http://myservice.com/foo") {
        get$ { response =>
          response.content.get
        } ~
        path$("/bar") { response =>
          get$ { response =>
            response.content.get
          }
        }
      }
    }

Similarly, if you're going to perform a lot of requests that all share the same or similar structure, then you can create your own combinator:

    def myService[T, S](r: HttpClientTransformer[T, S]): HttpClientTransformer[T, S] = {
      port(123) {
        path$("http://myservice.com/api/v1") {
          r 
        }
      }
    }

    ...
    val content = client {
      myService {
        get$ { response =>
          response.content.get
        }
      }
    }

Contrary to these toy examples, in real world usage, you would not simply return the content of the response. Rather, you'd extract out whatever information you need and transform it into the desired value.

### Testing

BlueEyes is built from the ground up to support automated, comprehensive, fast-running tests.

The testing framework is currently compatible with *Specs*, and extends the *Specification* trait to make testing services easy.

To test your services with *Specs*, you should extend *BlueEyesServiceSpecification* with whatever services you want to test. This trait, in turn, mixes in a variety of helper methods, including client transformer combinators.

    class EmailServicesSpec extends BlueEyesServiceSpecification[Array[Byte]] with EmailServices {
      path$("/emails/") {
        get$ { response =>
          response.status mustEqual(HttpStatus(OK))
        }
      } should "return OK status"
    }

These combinators produce very descriptive *Specs* messages, because they are fully aware of the path, HTTP method, and query string parameters you are using to invoke the service. This eliminates duplication between textual description and test logic, and makes you more productive.

### Execution

Services are run through a *server*. A "server" in this context refers to a *process*, not a *machine* -- any number of servers can run on the same physical machine.

To create a server, BlueEyes includes the *BlueEyesServer* trait, which is typically extended by an *object*. You can specify all the services you want the server to run just by mixing in the traits that build them. For example, the following code creates a server that runs four services:

    object AppServer extends BlueEyesServer with EmailServices with OrderProcessingServices with LoginServices with CatalogServices

A server created in this way has *start* and *stop* methods, which can be used for starting and stopping the services. The server also defines a *main* method that accepts a *--configFile* command-line option to indicate which configuration file should be used to configure the server and all the services.

    java -jar appserver.jar --configFile /etc/default/appserver.conf

A single server can run any number of services, although the recommended practice is to run each service on a separate server, on a separate port, and use a load balancer like *HAProxy* to unify the HTTP interface to the services. This approach confers a number of benefits:

 * Independent provisioning of services based on requirements (some services may be needed to maintain 100% uptime and thus may be replicated across instances and data centers, while others may not need such high-availability);
 * Independent scaling of services based on load; 
 * Isolation of services so that the crash of one service has no effect on others;
 * Independent deployment of services so that risk to production is minimized.

#### Server Configuration Options

### Augmentation

Services can be augmented in a variety of ways -- for example, with loggers, health monitors, and service locators. The augmentation facility is based on composition of so-called *service descriptor factories*, which are functions that accept a service context and return a service descriptor.

A service descriptor factory is at the heart of every service declaration. For example, take the following minimal service declaration:

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

The anonymous final argument passed to the *service* function is actually a service descriptor factory. That is, the argument is a function takes the context of the service, and returns a service descriptor, which describes the service lifecycle (startup, request handling, and shutdown).

BlueEyes ships with many useful service descriptor factory combinators that are designed to augment your service with additional features. For example, the *logging* combinator adds logging to your service:

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

The sections that follow describe the most common ways to augment your services.

#### Logging

BlueEyes provides a combinator that provides services with a logger that can be configured independently for each service.

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

A service's logger is configured through a *log* block inside the root config for the service.

#### Health Monitor

Health monitor allows services to export real-time metrics on health status, for use in continuous deployment.
 
The default health monitor automatically exports information on number of requests, number and type of errors, and length of requests.

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

Health metrics are exported in JSON form through an HTTP GET. For a particular service, the health can be queried at the following URL:

 * /blueeyes/services/[serviceName]/v[serviceMajorVersion]/health

For example:  */blueeyes/services/healthmon/v1/health*

#### Service Locator

If you have multiple services, and one service needs to consume another, you can use the service locator combinator. This combinator uses information in a config file to determine where to locate services, and provides a tailor-made client that can be used to communicate with them.

    trait ServiceLocatorDemo extends BlueEyesServiceBuilder {
      val serviceLocatorService = service ("email", "1.01") {
        serviceLocator { locator =>
          context => {
            request {
              path("/foo") {
                get { request: HttpRequest[String] =>
                  // Locate foo/v1 service and perform HTTP GET on /bar path
                  val content = locator("foo", "1.02.32") { fooService =>
                    fooService {
                      path$("/bar") {
                        get$ { response =>
                          response.content.get
                        }
                      }
                    }
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

## Data Exchange

### JSON

BlueEyes comes with the most fully-featured Scala library for JSON parsing, rendering, and manipulation.

## Persistence

### Cache

BlueEyes has several kinds of caches.

### MongoDB

BlueEyes has a full-featured Scala facade to MongoDB.

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
      <td>Michael Lagutko</td>    <td>Core platform, persistence</td>                                               <td></td>
    </tr>
    <tr>
      <td>Josh Hoak</td>          <td>HTTP headers</td>                                                             <td></td>
    </tr>
    <tr>
      <td>Jeff Simpson</td>       <td>Asynchronous HTTP client</td>                                                 <td></td>
    </tr>
    <tr>
      <td>Mike Conigliaro</td>    <td>Container/Deployment Manager for BlueEyes Services (unreleased)</td>         <td><a href="http://twitter.com/jdegoes">@mconigliaro</a></td>
    </tr>
  </tbody>
</table>

## License

Copyright (c) 2010-2011

Published under The MIT License