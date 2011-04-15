package blueeyes.core.service

import blueeyes.core.data.Chunk

trait HttpServerEngine { self: HttpRequestHandler[Chunk] =>
}