package blueeyes.concurrent

trait ActorExecutionStrategySequential{
  implicit val actorExecutionStrategy = new ActorExecutionStrategy {

    def execute1[T1, R](actor: Actor)(f: (T1) => R)(v1: T1)(response: Future[R]) = execute(actor)(() => f(v1))(response)

    def execute2[T1, T2, R](actor: Actor)(f: (T1, T2) => R)(v1: T1, v2: T2)(response: Future[R]) = execute(actor)(() => f(v1, v2))(response)

    def execute3[T1, T2, T3, R](actor: Actor)(f: (T1, T2, T3) => R)(v1: T1, v2: T2, v3: T3)(response: Future[R]) = execute(actor)(() => f(v1, v2, v3))(response)

    def execute4[T1, T2, T3, T4, R](actor: Actor)(f: (T1, T2, T3, T4) => R)(v1: T1, v2: T2, v3: T3, v4: T4)(response: Future[R]) = execute(actor)(() => f(v1, v2, v3, v4))(response)

    def execute5[T1, T2, T3, T4, T5, R](actor: Actor)(f: (T1, T2, T3, T4, T5) => R)(v1: T1, v2: T2, v3: T3, v4: T4, v5: T5)(response: Future[R]) = execute(actor)(() => f(v1, v2, v3, v4, v5))(response)

    def execute6[T1, T2, T3, T4, T5, T6, R](actor: Actor)(f: (T1, T2, T3, T4, T5, T6) => R)(v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6)(response: Future[R]) = execute(actor)(() => f(v1, v2, v3, v4, v5, v6))(response)

    def execute7[T1, T2, T3, T4, T5, T6, T7, R](actor: Actor)(f: (T1, T2, T3, T4, T5, T6, T7) => R)(v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7)(response: Future[R]) = execute(actor)(() => f(v1, v2, v3, v4, v5, v6, v7))(response)

    def execute8[T1, T2, T3, T4, T5, T6, T7, T8, R](actor: Actor)(f: (T1, T2, T3, T4, T5, T6, T7, T8) => R)(v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8)(response: Future[R]) = execute(actor)(() => f(v1, v2, v3, v4, v5, v6, v7, v8))(response)

    def execute9[T1, T2, T3, T4, T5, T6, T7, T8, T9, R](actor: Actor)(f: (T1, T2, T3, T4, T5, T6, T7, T8, T9) => R)(v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9)(response: Future[R]) = execute(actor)(() => f(v1, v2, v3, v4, v5, v6, v7, v8, v9))(response)

    def execute10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, R](actor: Actor)(f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10) => R)(v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10)(response: Future[R]) = execute(actor)(() => f(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10))(response)

    def execute11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, R](actor: Actor)(f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11) => R)(v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10, v11: T11)(response: Future[R]) = execute(actor)(() => f(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11))(response)

    def execute12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, R](actor: Actor)(f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12) => R)(v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10, v11: T11, v12: T12)(response: Future[R]) = execute(actor)(() => f(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12))(response)

    def execute13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, R](actor: Actor)(f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13) => R)(v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10, v11: T11, v12: T12, v13: T13)(response: Future[R]) = execute(actor)(() => f(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13))(response)

    def execute14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, R](actor: Actor)(f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14) => R)(v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10, v11: T11, v12: T12, v13: T13, v14: T14)(response: Future[R]) = execute(actor)(() => f(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14))(response)

    def execute15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, R](actor: Actor)(f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15) => R)(v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10, v11: T11, v12: T12, v13: T13, v14: T14, v15: T15)(response: Future[R]) = execute(actor)(() => f(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15))(response)

    def execute16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, R](actor: Actor)(f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16) => R)(v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10, v11: T11, v12: T12, v13: T13, v14: T14, v15: T15, v16: T16)(response: Future[R]) = execute(actor)(() => f(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16))(response)

    def execute17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, R](actor: Actor)(f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17) => R)(v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10, v11: T11, v12: T12, v13: T13, v14: T14, v15: T15, v16: T16, v17: T17)(response: Future[R]) = execute(actor)(() => f(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16, v17))(response)

    def execute18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, R](actor: Actor)(f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18) => R)(v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10, v11: T11, v12: T12, v13: T13, v14: T14, v15: T15, v16: T16, v17: T17, v18: T18)(response: Future[R]) = execute(actor)(() => f(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16, v17, v18))(response)

    def execute19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, R](actor: Actor)(f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19) => R)(v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10, v11: T11, v12: T12, v13: T13, v14: T14, v15: T15, v16: T16, v17: T17, v18: T18, v19: T19)(response: Future[R]) = execute(actor)(() => f(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16, v17, v18, v19))(response)

    def execute20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, R](actor: Actor)(f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20) => R)(v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10, v11: T11, v12: T12, v13: T13, v14: T14, v15: T15, v16: T16, v17: T17, v18: T18, v19: T19, v20: T20)(response: Future[R]) = execute(actor)(() => f(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16, v17, v18, v19, v20))(response)

    def execute21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, R](actor: Actor)(f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21) => R)(v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10, v11: T11, v12: T12, v13: T13, v14: T14, v15: T15, v16: T16, v17: T17, v18: T18, v19: T19, v20: T20, v21: T21)(response: Future[R]) = execute(actor)(() => f(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16, v17, v18, v19, v20, v21))(response)

    def execute22[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22, R](actor: Actor)(f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22) => R)(v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10, v11: T11, v12: T12, v13: T13, v14: T14, v15: T15, v16: T16, v17: T17, v18: T18, v19: T19, v20: T20, v21: T21, v22: T22)(response: Future[R]) = execute(actor)(() => f(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16, v17, v18, v19, v20, v21, v22))(response)

    def execute[R](actor: Actor)(f: () => R)(response: Future[R]){
      try {
        response.deliver(f())
      }
      catch {
        case e => response.cancel(e)
      }
    }
  }
}

object ActorStrategySequential{
  private lazy val futureDeliverySequential = new FutureDeliveryStrategySequential{}
  private lazy val actorExecutionSequential = new ActorExecutionStrategySequential{}

  implicit def futureDeliveryStrategy = futureDeliverySequential.futureDeliveryStrategy

  implicit def actorExecutionStrategy = actorExecutionSequential.actorExecutionStrategy
}