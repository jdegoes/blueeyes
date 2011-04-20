package blueeyes.core.http

import org.specs.Specification
import org.specs.ScalaCheck
import org.scalacheck._
import org.scalacheck.Prop.forAllNoShrink

class URISpec extends Specification with URIGen with ScalaCheck{
  "URL.toString" should{
    "be created using scheme" in {URI(Some("foo"), None, None, None, None, None, None).toString mustEqual("foo:")}
    "be created using scheme, userinfo and host" in {URI(Some("foo"), Some("john:smith"), Some("google"), None, None, None, None).toString mustEqual("foo://john:smith@google")}
    "be created using scheme, userinfo host and port" in {URI(Some("foo"), Some("john:smith"), Some("google"), Some(8080), None, None, None).toString mustEqual("foo://john:smith@google:8080")}
    "be created using scheme, userinfo host, port and path" in {URI(Some("foo"), Some("john:smith"), Some("google"), Some(8080), Some("/bar"), None, None).toString mustEqual("foo://john:smith@google:8080/bar")}
    "be created using scheme, userinfo host, port, path and query" in {URI(Some("foo"), Some("john:smith"), Some("google"), Some(8080), Some("/bar"), Some("query=value"), None).toString mustEqual("foo://john:smith@google:8080/bar?query=value")}
    "be created using scheme, userinfo host, port, path, query and fragment" in {URI(Some("foo"), Some("john:smith"), Some("google"), Some(8080), Some("/bar"), Some("query=value"), Some("fragment")).toString mustEqual("foo://john:smith@google:8080/bar?query=value#fragment")}
  }

  "URI" should{
    "parser uri" in {
      passTest(uri)
    }
  }

  "URI.parseEmails" should{
    "return the correct email name with a well-formed email and parse to None otherwise" in {
      URI.parseEmails("johnsmith@socialmedia.com ").get.toString mustEqual "johnsmith@socialmedia.com"
    }

    "return the correct (although weird) email" in {
      URI.parseEmails(" j.o.n.Sm.ith@so.cia.lmedia.com ").get.toString mustEqual "j.o.n.Sm.ith@so.cia.lmedia.com"
    }

    "parse non-email to None" in {
      URI.parseEmails("209h3094)(it092jom") mustEqual None
    }
  }

  private def passTest(gen: Gen[String]) = forAllNoShrink(gen)(n => URI(n).toString == n) must pass
}

import org.scalacheck._
import Gen._
import Arbitrary.arbitrary
trait URIGen{
  def option(gen: Gen[String]) = {
    for {
      exist <- arbitrary[Boolean]
      value <- gen
    } yield (if (exist) Some(value) else None)
  }
  def notEmptyString(length: Int) = listOfN(length, alphaChar).map(_.mkString(""))

  def schemeGen   = notEmptyString(4)
  def portGen     = listOfN(4, numChar).map(_.mkString(""))
  def hostGen     = {
    for {
      first  <- option(notEmptyString(10).map(_ + "."))
      second <- notEmptyString(10)
    } yield (first.map(_ + second).getOrElse(second))
  }
  def pathGen     = listOfN(4, notEmptyString(6).map("/" + _)).map(_.mkString(""))
  def queryGen   = for{
    key   <- notEmptyString(6)
    value <- notEmptyString(6)
  } yield (key + "=" + value)
  def fragmentGen = notEmptyString(10)
  def userInfoGen = for{
    withPass <- arbitrary[Boolean]
    user     <- notEmptyString(6)
    password <- notEmptyString(6)
  } yield (if (withPass) user + ":" + password else user)

  def uri = {
    for{
      scheme   <- option(schemeGen)
      userInfo <- option(userInfoGen)
      host     <- option(hostGen)
      port     <- option(portGen)
      path     <- option(pathGen)
      query    <- option(queryGen)
      fragment <- option(fragmentGen)
    } yield ( URI(scheme, userInfo, host, port.map(_.toInt), path, query, fragment).toString )
  }
}