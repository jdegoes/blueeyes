package blueeyes.core.service

import blueeyes.core.data.ByteChunk

trait HttpServerEngine { self: AsyncCustomHttpService[ByteChunk] =>
}