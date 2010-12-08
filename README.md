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

 * *config*. Every service gets its own separate config, namespaced by service name and major version.
 * *log*. Logging can be configured differently for different services.
 * *monitor*. Health monitor allows services to export real-time metrics on health status, for use in continuous deployment.

Service Construction
--------------------

Service Consumption
-------------------

Automated Testing
-----------------

Continuous Deployment
---------------------

License
-------

Copyright (c) 2010

Published under The MIT License