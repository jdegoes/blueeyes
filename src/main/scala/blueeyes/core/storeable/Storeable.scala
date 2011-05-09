package blueeyes.core.storeable

sealed trait Storeable

trait Record extends Storeable
trait Value extends Storeable