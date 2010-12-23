package blueeyes.persistence.cache

case class CacheSettings[K, V](
  expirationPolicy:         ExpirationPolicy,
  initialCapacity:          Int                     = 1000,
  evict:                    (K, V) => Unit          = (k: K, v: V) => (),
  maximumWeightedCapacity:  Int                     = 10000,
  weigh:                    V => Int                = (v: V) => 1
) 