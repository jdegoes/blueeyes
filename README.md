# Blue Eyes

Blue Eyes is a lightweight web 3.0 framework for the Scala programming language. The framework is designed to allow developers to quickly and easily create high-performing web services that embrace the machinery and language of HTTP.

Blue Eyes has been used in production across large clusters of instances deployed in cloud computing environments, reliably handling tens of thousands of requests a second.

The framework has been designed to meet all the following goals:

  * Stateless design, to achieve massive scalability;
  * Purely asynchronous request handling, to achieve extremely fast per-instance performance;
  * Highly composable, modular design;
  * Declarative service construction;
  * Baked in support for continuous deployment and automated testing.

Blue Eyes does not have any features for server-side generation of HTML, CSS, or JavaScript. Nor does Blue Eyes have any out of the box support for serving static files. Blue Eyes is intended *only* for creating web services which can be consumed using HTTP clients.

Those looking for a traditional web framework for the Scala programming language are directed to the [Lift Web Framework](http://www.liftweb.net/).

## Maven

<table>
  <thead>
    <tr>
      <td>Name</td>             <td>Value</td>
    </tr>
  </thead>
  <tbody>
    <tr>
      <td>repository</td>       <td>http://oss.sonatype.org/content/repositories/releases</td>
    </tr>
    <tr>
      <td>group id</td>         <td>com.github.blueeyes</td>
    </tr>
    <tr>
      <td>artifact id</td>      <td>blueeyes</td>
    </tr>
    <tr>
      <td>version</td>          <td>0.1.32</td>
    </tr>
  </tbody>
</table>

### SBT

    val sonatypeRepository = MavenRepository("Sonatype Releases", "http://oss.sonatype.org/content/repositories/releases")
    
    val blueeyesRelease = "com.github.blueeyes" % "blueeyes" % "0.1.32" % "compile"

## Origins

Blue Eyes is loosely inspired by the Ruby library *Sinatra* and the Scala library *Scalatra*, which both allow developers to efficient produce RESTful web services.

Blue Eyes aims for the same or higher level of productivity as these libraries, but with a more functional design, much higher performance, and compatibility with the rigorous demands of continuous deployment.

## Introduction

The fundamental concept in Blue Eyes is the *service*. A *service* responds to requests. Every service is uniquely identified by a name and a version.

A service generally goes through three distinct phases:

  1. *Startup*. The service performs any setup operations required to perform its duties, such as loading data.
  2. *Request*. The service responds to requests.
  3. *Shutdown*. The service performs any cleanup operations, such as disposing of resources.

In the request phase, services typically handle different HTTP verbs (GET, POST, PUT, DELETE) on different paths, accepting and producing different mime types.

Services are generally built using *BlueEyesServiceBuilder*, which mixes in numerous traits to make building services fast and painless.

The following code builds an e-mail service, together with a server capable of running the service from the command-line:

     trait EmailServices extends BlueEyesServiceBuilder {
       val emailService = service("email", "1.32") { context =>
         startup {
           Future.async {
             // return state
             ...
           }
         } ->
         request { state =>
           path("/foo") {
             contentType(application/json) {
                get { request =>
                  ...
                } ~
                post { request =>
                  ...
                }
             } ~     
             path("/bar") {
               ...
             }
           }
         } ->
         shutdown { state =>
            ...
         }
       }
     }
     object EmailServer extends BlueEyesServer with EmailServices

Services are automatically provided with *context*, which provides a bundle of functionality essential to every service:

 * *config*. Every service gets its own separate config, namespaced by service name and major version (*services.[serviceName].v[serviceMajorVersion]*)
 * *serviceName*. Name of the service.
 * *serviceVersion*. Version of the service.

## Services

### Construction

### Consumption

The dual of providing HTTP services is consuming them. Blue Eyes includes a high-performance, asynchronous HTTP client that is unified with the rest of the Blue Eyes stack.

The core client interface is *HttpClient*, which is a partial function from request to a future of response. The *apply* method is seldom used directly. Instead, Blue Eyes provides so-called *client transformer combinators*, which are functions that map an *HttpClient* into a future of some value.

For the most part, the client transformer combinators mirror the request handler combinators, except all methods are suffixed with a dollar sign character ('$'). This means you can consume services using a syntax almost identical to the one you use to construct them.

Given a reference to *client*, you could perform a simple HTTP GET on the path "/foo" with the following code:

    val content = client.exec path$("http://myservice.com/foo") {
      get$ { response =>
        response.content.get
      }
    }

The great power of this design lies in its composability. If you wanted to perform an HTTP GET on path "/foo/bar" at the same time as the first GET, without duplicating all the same code, you could do so using the client transformer join operator ('~'):

    val (fooContent, fooBarContent) = client.exec {
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
        path$("http://myservice.com") {
          r 
        }
      }
    }

    ...
    client.exec {
      myService {
        get$ { response =>
          response.content.get
        }
      }
    }

Contrary to these toy examples, in real world usage, you would not simply return the content of the response. Rather, you'd extract out whatever information you need and transform it into the desired value.

### Testing

Blue Eyes is built from the ground up to support automated, comprehensive, fast-running tests.

The testing framework is currently compatible with *Specs*, and extends the *Specification* trait to make testing services easy.

To test your services with *Specs*, you should extend *BlueEyesServiceSpecification* with whatever services you want to test. This trait, in turn, mixes in a variety of helper methods, including client transformer combinators.

    class EmailServicesSpec extends BlueEyesServiceSpecification[Array[Byte]] with EmailServices {
      path$("/foo") {
        get$ { response =>
          response.status mustEqual(HttpStatus(OK))
        }
      } should "return OK status"
    }

These combinators produce very descriptive *Specs* messages, because they are fully aware of the path, HTTP method, and query string parameters you are using to invoke the service. This eliminates duplication between textual description and test logic, and makes you more productive.

### Running

Services are run through a *server*. A "server" in this context refers to a *process*, not a *machine* -- any number of servers can run on the same physical machine.

To create a server, Blue Eyes includes the *BlueEyesServer* trait, which is typically extended by an *object*. You can specify all the services you want the server to run just by mixing in the traits that build them. For example, the following code creates a server that runs four services:

    object AppServer extends BlueEyesServer with EmailServices with OrderProcessingServices with LoginServices with CatalogServices

A server created in this way has *start* and *stop* methods, which can be used for starting and stopping the services. The server also defines a *main* method that accepts a *--configFile* command-line option to indicate which configuration file should be used to configure the server and all the services.

    java -jar appserver.jar --configFile /etc/default/appserver.conf

A single server can run any number of services, although the recommended practice is to run each service on a separate server, on a separate port, and use a load balancer like *HAProxy* to unify the HTTP interface to the services. This approach confers a number of benefits:

 * Independent provisioning of services based on requirements (some services may be needed to maintain 100% uptime and thus may be replicated across instances and data centers, while others may not need such high-availability);
 * Independent scaling of services based on load; 
 * Isolation of services so that the crash of one services has no effect on others;
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

Blue Eyes ships with many useful service descriptor factory combinators that are designed to augment your service with additional features. For example, the *logging* combinator adds logging to your service:

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

Blue Eyes provides a combinator that provides services with a logger that can be configured independently for each service.

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

## Data

### JSON

## Persistence

### MongoDB

## Continuous Deployment

Blue Eyes is designed to support the lean development practice of _continuous deployment_. In continuous deployment, the team can safely deploy code as often as required by business needs -- even many times a day.

In order for a team to practice continuous deployment successfully, two critical requirements must be met:

 1. The entire code base needs to have comprehensive, fast automated tests, typically at the unit level (integration and system tests can be included only if they run quickly).
 2. The application needs to export real-time health metrics for use in detecting quality issues post-deployment (for the most part, these metrics supplant integration and system tests)

On top of this foundation, a continuous deployment system can be built, which does incremental deployments with health checks, and which either backs out defective releases automatically, or makes it easy for team members to do so.

Blue Eyes provides support for both pillars:

 1. Blue Eyes makes testing web services extremely easy, and the tests do not start a real server so they run very quickly.
 2. Blue Eyes makes it easy to export real-time health metrics (and, if you use health monitor, automatically exports all the critical metrics).

## Team

<table>
  <thead>
    <tr>
      <td>Name</td>               <td>Role</td>                                                                    <td>Twitter</td>
    </tr>
  </thead>
  <tbody>
    <tr>
      <td>John A. De Goes</td>    <td>Author &amp; architect, core platform</td>                                    <td>@jdegoes</td>
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
      <td>Mike Conigliaro</td>    <td>Container/Deployment Manager for Blue Eyes Services (unreleased)</td>         <td>@mconigliaro</td>
    </tr>
  </tbody>
</table>

## License

Copyright (c) 2010-2011

Published under The MIT License