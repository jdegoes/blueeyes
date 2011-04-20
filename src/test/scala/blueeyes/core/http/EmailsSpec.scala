package blueeyes.core.http

import org.specs.Specification

class EmailsSpec extends Specification{
  "Emails" should{
    "return the correct email name with a well-formed email" in {
      Emails("johnsmith@socialmedia.com ") must beSome(Email("johnsmith@socialmedia.com"))
    }

    "return the correct (although weird) email" in {
      Emails(" j.o.n.Sm.ith@so.cia.lmedia.com ") must beSome(Email("j.o.n.Sm.ith@so.cia.lmedia.com"))
    }

    "parse non-email to None" in {
      Emails("209h3094)(it092jom") mustEqual None
    }
  }

}