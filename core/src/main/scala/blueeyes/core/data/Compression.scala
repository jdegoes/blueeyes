package blueeyes.core.data

trait Compression[A] {
  def apply(data: A): A
}

// vim: set ts=4 sw=4 et:
