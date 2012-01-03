package blueeyes.concurrent


trait ReadWriteLock{
  private val lock = new java.util.concurrent.locks.ReentrantReadWriteLock

  def writeLock[S](f: => S): S = {
    lock.writeLock.lock()
    try {
      f
    } finally {
      lock.writeLock.unlock()
    }
  }

  def readLock[S](f: => S): S = {
    lock.readLock.lock()
    try {
      f
    } finally {
      lock.readLock.unlock()
    }
  }
}
