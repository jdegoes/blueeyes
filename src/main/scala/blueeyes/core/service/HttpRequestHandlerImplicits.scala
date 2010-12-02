package blueeyes.core.service

import blueeyes.core.http.{HttpRequest, HttpResponse}
import blueeyes.util.Future
import blueeyes.core.data._

trait HttpRequestHandlerImplicits {
  implicit def partialFunctionRequestHandlerCombinatorSugar[A, B](p1: PartialFunction[A, B]) = new {
    def ~ (p2: PartialFunction[A, B]): PartialFunction[A, B] = p1.orElse(p2)
  }
  
  implicit def notRequestHandlerCombinatorSugar[T, S](r: HttpRequestHandler2[T, S] => HttpRequestHandler2[T, S]) = new {
    def unary_! : HttpRequestHandler2[T, S] => HttpRequestHandler2[T, S] = (handler: HttpRequestHandler2[T, S]) => {
      val r2 = r(handler)
      
      new HttpRequestHandler2[T, S] {
        def isDefinedAt(r: HttpRequest[T]): Boolean = !r2.isDefinedAt(r)
      
        def apply(r: HttpRequest[T]): Future[HttpResponse[S]] = r2(r)
      }
    }
  }
  
  implicit def orRequestHandlerCombinatorSugar[T, S](r1: HttpRequestHandler2[T, S] => HttpRequestHandler2[T, S]) = new {
    def | (r2: HttpRequestHandler2[T, S] => HttpRequestHandler2[T, S])(h: HttpRequestHandler2[T, S]): HttpRequestHandler2[T, S] = r1(h).orElse(r2(h))
    
    def || (r2: HttpRequestHandler2[T, S] => HttpRequestHandler2[T, S])(h: HttpRequestHandler2[T, S]): HttpRequestHandler2[T, S] = r1(h).orElse(r2(h))
  }
  
  implicit def orRequestHandlerCombinatorSugarExtractor1[T, S, U1, V1](r1: (U1 => HttpRequestHandler2[T, S]) => HttpRequestHandler2[T, S]) = new {
    def | (r2: (V1 => HttpRequestHandler2[T, S]) => HttpRequestHandler2[T, S])(h: Either[U1, V1] => HttpRequestHandler2[T, S]): HttpRequestHandler2[T, S] = {
      r1(u1 => h(Left(u1))).orElse(r2(v1 => h(Right(v1))))
    }
    
    def || (r2: (V1 => HttpRequestHandler2[T, S]) => HttpRequestHandler2[T, S])(h: Either[U1, V1] => HttpRequestHandler2[T, S]): HttpRequestHandler2[T, S] = | (r2)(h)
  }
  
  implicit def orRequestHandlerCombinatorSugarExtractor2[T, S, U1, U2, V1, V2](r1: ((U1, U2) => HttpRequestHandler2[T, S]) => HttpRequestHandler2[T, S]) = new {
    def | (r2: ((V1, V2) => HttpRequestHandler2[T, S]) => HttpRequestHandler2[T, S])(h: Either[(U1, U2), (V1, V2)] => HttpRequestHandler2[T, S]): HttpRequestHandler2[T, S] = {
      r1((u1, u2) => h(Left((u1, u2)))).orElse(r2((v1, v2) => h(Right((v1, v2)))))
    }
    
    def || (r2: ((V1, V2) => HttpRequestHandler2[T, S]) => HttpRequestHandler2[T, S])(h: Either[(U1, U2), (V1, V2)] => HttpRequestHandler2[T, S]): HttpRequestHandler2[T, S] = | (r2)(h)
  }
  
  implicit def orRequestHandlerCombinatorSugarExtractor3[T, S, U1, U2, U3, V1, V2, V3](r1: ((U1, U2, U3) => HttpRequestHandler2[T, S]) => HttpRequestHandler2[T, S]) = new {
    def | (r2: ((V1, V2, V3) => HttpRequestHandler2[T, S]) => HttpRequestHandler2[T, S])(h: Either[(U1, U2, U3), (V1, V2, V3)] => HttpRequestHandler2[T, S]): HttpRequestHandler2[T, S] = {
      r1((u1, u2, u3) => h(Left((u1, u2, u3)))).orElse(r2((v1, v2, v3) => h(Right((v1, v2, v3)))))
    }
    
    def || (r2: ((V1, V2, V3) => HttpRequestHandler2[T, S]) => HttpRequestHandler2[T, S])(h: Either[(U1, U2, U3), (V1, V2, V3)] => HttpRequestHandler2[T, S]): HttpRequestHandler2[T, S] = | (r2)(h)
  }
  
  implicit def orRequestHandlerCombinatorSugarExtractor4[T, S, U1, U2, U3, U4, V1, V2, V3, V4](r1: ((U1, U2, U3, U4) => HttpRequestHandler2[T, S]) => HttpRequestHandler2[T, S]) = new {
    def | (r2: ((V1, V2, V3, V4) => HttpRequestHandler2[T, S]) => HttpRequestHandler2[T, S])(h: Either[(U1, U2, U3, U4), (V1, V2, V3, V4)] => HttpRequestHandler2[T, S]): HttpRequestHandler2[T, S] = {
      r1((u1, u2, u3, u4) => h(Left((u1, u2, u3, u4)))).orElse(r2((v1, v2, v3, v4) => h(Right((v1, v2, v3, v4)))))
    }
    
    def || (r2: ((V1, V2, V3, V4) => HttpRequestHandler2[T, S]) => HttpRequestHandler2[T, S])(h: Either[(U1, U2, U3, U4), (V1, V2, V3, V4)] => HttpRequestHandler2[T, S]): HttpRequestHandler2[T, S] = | (r2)(h)
  }
  
  implicit def orRequestHandlerCombinatorSugarExtractor5[T, S, U1, U2, U3, U4, U5, V1, V2, V3, V4, V5](r1: ((U1, U2, U3, U4, U5) => HttpRequestHandler2[T, S]) => HttpRequestHandler2[T, S]) = new {
    def | (r2: ((V1, V2, V3, V4, V5) => HttpRequestHandler2[T, S]) => HttpRequestHandler2[T, S])(h: Either[(U1, U2, U3, U4, U5), (V1, V2, V3, V4, V5)] => HttpRequestHandler2[T, S]): HttpRequestHandler2[T, S] = {
      r1((u1, u2, u3, u4, u5) => h(Left((u1, u2, u3, u4, u5)))).orElse(r2((v1, v2, v3, v4, v5) => h(Right((v1, v2, v3, v4, v5)))))
    }
    
    def || (r2: ((V1, V2, V3, V4, V5) => HttpRequestHandler2[T, S]) => HttpRequestHandler2[T, S])(h: Either[(U1, U2, U3, U4, U5), (V1, V2, V3, V4, V5)] => HttpRequestHandler2[T, S]): HttpRequestHandler2[T, S] = | (r2)(h)
  }
  
  implicit def andRequestHandlerCombinatorSugar[T, S](r1: HttpRequestHandler2[T, S] => HttpRequestHandler2[T, S]) = new {
    def & (r2: HttpRequestHandler2[T, S] => HttpRequestHandler2[T, S])(h: HttpRequestHandler2[T, S]): HttpRequestHandler2[T, S] = new HttpRequestHandler2[T, S] {
      def isDefinedAt(r: HttpRequest[T]): Boolean = r1(h).isDefinedAt(r) && r2(h).isDefinedAt(r)
      
      def apply(r: HttpRequest[T]): Future[HttpResponse[S]] = r1(h).apply(r)
    }
    
    def && (r2: HttpRequestHandler2[T, S] => HttpRequestHandler2[T, S])(h: HttpRequestHandler2[T, S]): HttpRequestHandler2[T, S] = r1.andThen(r2)(h)
  }
  
  implicit def andRequestHandlerCombinatorSugarExtractor1[T, S, U1, V1](r1: (U1 => HttpRequestHandler2[T, S]) => HttpRequestHandler2[T, S]) = new {
    def & (r2: (V1 => HttpRequestHandler2[T, S]) => HttpRequestHandler2[T, S])(h: (U1, V1) => HttpRequestHandler2[T, S]): HttpRequestHandler2[T, S] = {
      r1 { u1 => 
        r2 { v1 =>
          h(u1, v1)
        }
      }
    }
    
    def && (r2: (V1 => HttpRequestHandler2[T, S]) => HttpRequestHandler2[T, S])(h: (U1, V1) => HttpRequestHandler2[T, S]): HttpRequestHandler2[T, S] = & (r2)(h)
  }
}
object HttpRequestHandlerImplicits extends HttpRequestHandlerImplicits