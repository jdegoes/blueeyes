package blueeyes.util

trait PartialFunctionCombinators {
  implicit def partialFunctionRequestHandlerCombinatorSugar[A, B](p1: PartialFunction[A, B]) = new PartialFunctionRequestHandlerCombinatorSugar(p1)
  class PartialFunctionRequestHandlerCombinatorSugar[A, B](p1: PartialFunction[A, B]) {
    def ~ (p2: PartialFunction[A, B]): PartialFunction[A, B] = p1.orElse(p2)
  }
  
  implicit def notPartialFunctionCombinatorSugar[T, S](r: PartialFunction[T, S] => PartialFunction[T, S]) = new NotPartialFunctionCombinatorSugar(r)
  class NotPartialFunctionCombinatorSugar[T, S](r: PartialFunction[T, S] => PartialFunction[T, S]) {
    def unary_! : PartialFunction[T, S] => PartialFunction[T, S] = (handler: PartialFunction[T, S]) => {
      val r2 = r(handler)

      new PartialFunction[T, S] {
        def isDefinedAt(r: T): Boolean = !r2.isDefinedAt(r)

        def apply(r: T): S = r2(r)
      }
    }
  }

  implicit def orPartialFunctionCombinatorSugar[T, S](r1: PartialFunction[T, S] => PartialFunction[T, S]) = new OrPartialFunctionCombinatorSugar(r1)
  class OrPartialFunctionCombinatorSugar[T, S](r1: PartialFunction[T, S] => PartialFunction[T, S]) {
    def | (r2: PartialFunction[T, S] => PartialFunction[T, S])(h: PartialFunction[T, S]): PartialFunction[T, S] = r1(h).orElse(r2(h))

    def || (r2: PartialFunction[T, S] => PartialFunction[T, S])(h: PartialFunction[T, S]): PartialFunction[T, S] = r1(h).orElse(r2(h))
  }

  implicit def orPartialFunctionCombinatorSugarExtractor1[T, S, U1](r1: (U1 => PartialFunction[T, S]) => PartialFunction[T, S]) = new OrPartialFunctionCombinatorSugarExtractor1(r1)
  class OrPartialFunctionCombinatorSugarExtractor1[T, S, U1](r1: (U1 => PartialFunction[T, S]) => PartialFunction[T, S]) {
    def | (r2: (U1 => PartialFunction[T, S]) => PartialFunction[T, S]) = (h: U1 => PartialFunction[T, S]) => {
      r1(u1 => h(u1)).orElse(r2(v1 => h(v1)))
    }

    def || (r2: (U1 => PartialFunction[T, S]) => PartialFunction[T, S]) = (h: U1 => PartialFunction[T, S]) => | (r2)(h)
  }

  implicit def andPartialFunctionCombinatorSugarExtractor1[T, S, U1, V1](r1: (U1 => PartialFunction[T, S]) => PartialFunction[T, S]) = new AndPartialFunctionCombinatorSugarExtractor1(r1)
  class AndPartialFunctionCombinatorSugarExtractor1[T, S, U1, V1](r1: (U1 => PartialFunction[T, S]) => PartialFunction[T, S]) {
    def & (r2: (V1 => PartialFunction[T, S]) => PartialFunction[T, S]) = (h: (U1, V1) => PartialFunction[T, S]) => {
      r1 { u1 =>
        r2 { v1 =>
          h(u1, v1)
        }
      }
    }

    def && (r2: (V1 => PartialFunction[T, S]) => PartialFunction[T, S]) = (h: (U1, V1) => PartialFunction[T, S]) => & (r2)(h)
  }

  implicit def andPartialFunctionCombinatorSugarExtractor2[T, S, U1, U2](r1: ((U1, U2) => PartialFunction[T, S]) => PartialFunction[T, S]) = new AndPartialFunctionCombinatorSugarExtractor2(r1)
  class AndPartialFunctionCombinatorSugarExtractor2[T, S, U1, U2](r1: ((U1, U2) => PartialFunction[T, S]) => PartialFunction[T, S]) {
    def & [V1](r2: (V1 => PartialFunction[T, S]) => PartialFunction[T, S]) = (h: (U1, U2, V1) => PartialFunction[T, S]) => {
      r1 { (u1, u2) =>
        r2 { v1 =>
          h(u1, u2, v1)
        }
      }
    }

    def && [V1](r2: (V1 => PartialFunction[T, S]) => PartialFunction[T, S]) = (h: (U1, U2, V1) => PartialFunction[T, S]) => & (r2)(h)
  }

  implicit def andPartialFunctionCombinatorSugarExtractor3[T, S, U1, U2, U3](r1: ((U1, U2, U3) => PartialFunction[T, S]) => PartialFunction[T, S]) = new AndPartialFunctionCombinatorSugarExtractor3(r1)
  class AndPartialFunctionCombinatorSugarExtractor3[T, S, U1, U2, U3](r1: ((U1, U2, U3) => PartialFunction[T, S]) => PartialFunction[T, S]) {
    def & [V1](r2: (V1 => PartialFunction[T, S]) => PartialFunction[T, S]) = (h: (U1, U2, U3, V1) => PartialFunction[T, S]) => {
      r1 { (u1, u2, u3) =>
        r2 { v1 =>
          h(u1, u2, u3, v1)
        }
      }
    }

    def && [V1](r2: (V1 => PartialFunction[T, S]) => PartialFunction[T, S]) = (h: (U1, U2, U3, V1) => PartialFunction[T, S]) => & (r2)(h)
  }

  implicit def andPartialFunctionCombinatorSugarExtractor4[T, S, U1, U2, U3, U4](r1: ((U1, U2, U3, U4) => PartialFunction[T, S]) => PartialFunction[T, S]) = new AndPartialFunctionCombinatorSugarExtractor4(r1)
  class AndPartialFunctionCombinatorSugarExtractor4[T, S, U1, U2, U3, U4](r1: ((U1, U2, U3, U4) => PartialFunction[T, S]) => PartialFunction[T, S]) {
    def & [V1](r2: (V1 => PartialFunction[T, S]) => PartialFunction[T, S]) = (h: (U1, U2, U3, U4, V1) => PartialFunction[T, S]) => {
      r1 { (u1, u2, u3, u4) =>
        r2 { v1 =>
          h(u1, u2, u3, u4, v1)
        }
      }
    }

    def && [V1](r2: (V1 => PartialFunction[T, S]) => PartialFunction[T, S]) = (h: (U1, U2, U3, U4, V1) => PartialFunction[T, S]) => & (r2)(h)
  }

  implicit def andPartialFunctionCombinatorSugarExtractor5[T, S, U1, U2, U3, U4, U5](r1: ((U1, U2, U3, U4, U5) => PartialFunction[T, S]) => PartialFunction[T, S]) = new AndPartialFunctionCombinatorSugarExtractor5(r1)
  class AndPartialFunctionCombinatorSugarExtractor5[T, S, U1, U2, U3, U4, U5](r1: ((U1, U2, U3, U4, U5) => PartialFunction[T, S]) => PartialFunction[T, S]) {
    def & [V1](r2: (V1 => PartialFunction[T, S]) => PartialFunction[T, S]) = (h: (U1, U2, U3, U4, U5, V1) => PartialFunction[T, S]) => {
      r1 { (u1, u2, u3, u4, u5) =>
        r2 { v1 =>
          h(u1, u2, u3, u4, u5, v1)
        }
      }
    }

    def && [V1](r2: (V1 => PartialFunction[T, S]) => PartialFunction[T, S]) = (h: (U1, U2, U3, U4, U5, V1) => PartialFunction[T, S]) => & (r2)(h)
  }
}