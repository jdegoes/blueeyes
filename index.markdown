---
title: BlueEyes
layout: front
---
BlueEyes is a web framework for the Scala programming language that is focused on building high-performance web services. This means:

- Purely asynchronous request handling, for scalability to high requests-per-second and many simultaneous open connections;
- Highly composable, modular design;
- Declarative service construction;
- Support for continuous deployment and automated testing;
- Idiomatic Scala interfaces to highly-scalable databases such as MongoDB.

## Examples

The core of BlueEyes is a DSL for filtering HTTP requests. Want to match just `POST`s to `/api/v1/login`? Here's how:

{% highlight scala %}
path("/api/v1/login") {
  post {
    // Do something
  }
}
{% endhighlight %}
