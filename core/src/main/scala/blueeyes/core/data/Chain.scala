package blueeyes.core.data

import akka.dispatch.ExecutionContext
import akka.dispatch.Promise
import java.nio.ByteBuffer

class Chain(val promise: Promise[Option[(ByteBuffer, Chain)]])
object Chain {
  def incomplete(implicit executor: ExecutionContext) = new Chain(Promise[Option[(ByteBuffer, Chain)]]())
  def complete(implicit executor: ExecutionContext) = new Chain(Promise.successful(None))
}


// vim: set ts=4 sw=4 et:
