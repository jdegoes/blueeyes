Blue Eyes
=========

Blue Eyes is a lightweight web 3.0 framework for the Scala programming language. The framework is designed to allow developers to quickly and easily create high-performing web services that embrace the machinery and language of HTTP.

The framework has been designed to meet all the following goals:

  * Stateless design, to achieve massive scalability;
  * Purely asynchronous request handling, to achieve extremely fast per-instance performance;
  * Highly composable, modular design;
  * Declarative service construction;
  * Baked in support for continuous deployment and automated testing.

Blue Eyes does not have any features for generation of static HTML pages from templates. Nor does Blue Eyes have any out of the box support for serving static HTML files. Blue Eyes is intended *only* for creating web services which can be consumed using HTTP clients.

Origins
-------

Blue Eyes is loosely inspired by the Ruby library Sinatra and the Scala library Scalatra, which both allow developers to efficient produce RESTful web services.

Blue Eyes aims for the same or higher level of productivity as these libraries, but with a more functional design, much higher performance, and compatibility with the rigorous demands of continuous deployment.

Introduction
------------

The fundamental concept in Blue Eyes is the *service*. A *service* responds to requests. Every service is uniquely identified by a name and a version.

A service generally goes through three distinct phases:

  1. *Startup*. The service performs any setup operations required to perform its duties, such as loading data.
  2. *Request*. The service responds to requests.
  3. *Shutdown*. The service performs any cleanup operations, such as closing files.

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

Logging
--------------------
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

Health Monitor
--------------------
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

Service Construction
--------------------

Service Consumption
-------------------

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

Automated Testing
-----------------
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

Continuous Deployment
---------------------

Blue Eyes is designed to support the lean practice of _continuous deployment_. In continuous deployment, the goal is to attain the ability to safely deploy code as often as required by business needs -- even many times a day.

In order for a team to practice continuous deployment successfully, two critical conditions must be met:

 1. The entire code base needs to have comprehensive, fast automated tests, typically at the unit level (integration and system tests can be included only if they run quickly).
 2. The application needs to export real-time health metrics for use in detecting quality issues post-deployment (for the most part, these metrics supplant integration and system tests)

On top of this foundation, a continuous deployment system can be built, which does rolling deployments with health checks, and either backs out defective releases automatically, or makes it easy for team members to do so.

Blue Eyes provides support for both pillars:

 1. Blue Eyes makes testing web services extremely easy, and the tests do not start a real server so they run very quickly.
 2. Blue Eyes makes it easy to export real-time health metrics (and, if you use health monitor, automatically exports all the critical metrics).

Team
-------

<table>
  <tbody>
    <tr>
      <td>John A. De Goes</td>    <td>Author &amp; architect, core platform</td>
    </tr>
    <tr>
      <td>Michael Lagutko</td>    <td>Core platform, persistence</td>
    </tr>
    <tr>
      <td>Josh Hoak</td>          <td>HTTP headers</td>
    </tr>
    <tr>
      <td>Jeff Simpson</td>       <td>Asynchronous HTTP client</td>
    </tr>
    <tr>
      <td>Mike Conigliaro</td>    <td>Container/Deployment Manager for Blue Eyes Services (unreleased)</td>
    </tr>
  </tbody>
</table>

License
-------

Copyright (c) 2010

Published under The MIT License