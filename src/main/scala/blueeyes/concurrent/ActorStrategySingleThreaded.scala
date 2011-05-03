package blueeyes.concurrent

import java.util.concurrent.Executors

trait ActorExecutionStrategySingleThreaded {
  private val executor           = Executors.newSingleThreadExecutor
  private val sequential         = new ActorExecutionStrategySequential { }
  private val sequentialStrategy = sequential.actorExecutionStrategy

  implicit val actorExecutionStrategy = new ActorExecutionStrategy {

    def execute[R](f: () => R)(response: Future[R]) = execute(sequentialStrategy.execute(f) _)(response)

    def execute1[T1, R](f: (T1) => R)(v1: T1)(response: Future[R]) = execute(sequentialStrategy.execute1(f)(v1) _)(response)

    def execute2[T1, T2, R](f: (T1, T2) => R)(v1: T1, v2: T2)(response: Future[R]) = execute(sequentialStrategy.execute2(f)(v1, v2) _)(response)

    def execute3[T1, T2, T3, R](f: (T1, T2, T3) => R)(v1: T1, v2: T2, v3: T3)(response: Future[R]) = execute(sequentialStrategy.execute3(f)(v1, v2, v3) _)(response)

    def execute4[T1, T2, T3, T4, R](f: (T1, T2, T3, T4) => R)(v1: T1, v2: T2, v3: T3, v4: T4)(response: Future[R]) = execute(sequentialStrategy.execute4(f)(v1, v2, v3, v4) _)(response)

    def execute5[T1, T2, T3, T4, T5, R](f: (T1, T2, T3, T4, T5) => R)(v1: T1, v2: T2, v3: T3, v4: T4, v5: T5)(response: Future[R]) = execute(sequentialStrategy.execute5(f)(v1, v2, v3, v4, v5) _)(response)

    def execute6[T1, T2, T3, T4, T5, T6, R](f: (T1, T2, T3, T4, T5, T6) => R)(v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6)(response: Future[R]) = execute(sequentialStrategy.execute6(f)(v1, v2, v3, v4, v5, v6) _)(response)

    def execute7[T1, T2, T3, T4, T5, T6, T7, R](f: (T1, T2, T3, T4, T5, T6, T7) => R)(v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7)(response: Future[R]) = execute(sequentialStrategy.execute7(f)(v1, v2, v3, v4, v5, v6, v7) _)(response)

    def execute8[T1, T2, T3, T4, T5, T6, T7, T8, R](f: (T1, T2, T3, T4, T5, T6, T7, T8) => R)(v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8)(response: Future[R]) = execute(sequentialStrategy.execute8(f)(v1, v2, v3, v4, v5, v6, v7, v8) _)(response)

    def execute9[T1, T2, T3, T4, T5, T6, T7, T8, T9, R](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9) => R)(v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9)(response: Future[R]) = execute(sequentialStrategy.execute9(f)(v1, v2, v3, v4, v5, v6, v7, v8, v9) _)(response)

    def execute10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, R](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10) => R)(v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10)(response: Future[R]) = execute(sequentialStrategy.execute10(f)(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10) _)(response)

    def execute11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, R](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11) => R)(v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10, v11: T11)(response: Future[R]) = execute(sequentialStrategy.execute11(f)(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11) _)(response)

    def execute12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, R](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12) => R)(v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10, v11: T11, v12: T12)(response: Future[R]) = execute(sequentialStrategy.execute12(f)(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12) _)(response)

    def execute13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, R](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13) => R)(v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10, v11: T11, v12: T12, v13: T13)(response: Future[R]) = execute(sequentialStrategy.execute13(f)(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13) _)(response)

    def execute14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, R](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14) => R)(v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10, v11: T11, v12: T12, v13: T13, v14: T14)(response: Future[R]) = execute(sequentialStrategy.execute14(f)(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14) _)(response)

    def execute15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, R](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15) => R)(v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10, v11: T11, v12: T12, v13: T13, v14: T14, v15: T15)(response: Future[R]) = execute(sequentialStrategy.execute15(f)(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15) _)(response)

    def execute16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, R](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16) => R)(v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10, v11: T11, v12: T12, v13: T13, v14: T14, v15: T15, v16: T16)(response: Future[R]) = execute(sequentialStrategy.execute16(f)(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16) _)(response)

    def execute17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, R](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17) => R)(v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10, v11: T11, v12: T12, v13: T13, v14: T14, v15: T15, v16: T16, v17: T17)(response: Future[R]) = execute(sequentialStrategy.execute17(f)(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16, v17) _)(response)

    def execute18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, R](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18) => R)(v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10, v11: T11, v12: T12, v13: T13, v14: T14, v15: T15, v16: T16, v17: T17, v18: T18)(response: Future[R]) = execute(sequentialStrategy.execute18(f)(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16, v17, v18) _)(response)

    def execute19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, R](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19) => R)(v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10, v11: T11, v12: T12, v13: T13, v14: T14, v15: T15, v16: T16, v17: T17, v18: T18, v19: T19)(response: Future[R]) = execute(sequentialStrategy.execute19(f)(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16, v17, v18, v19) _)(response)

    def execute20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, R](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20) => R)(v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10, v11: T11, v12: T12, v13: T13, v14: T14, v15: T15, v16: T16, v17: T17, v18: T18, v19: T19, v20: T20)(response: Future[R]) = execute(sequentialStrategy.execute20(f)(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16, v17, v18, v19, v20) _)(response)

    def execute21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, R](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21) => R)(v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10, v11: T11, v12: T12, v13: T13, v14: T14, v15: T15, v16: T16, v17: T17, v18: T18, v19: T19, v20: T20, v21: T21)(response: Future[R]) = execute(sequentialStrategy.execute21(f)(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16, v17, v18, v19, v20, v21) _)(response)

    def execute22[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22, R](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22) => R)(v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10, v11: T11, v12: T12, v13: T13, v14: T14, v15: T15, v16: T16, v17: T17, v18: T18, v19: T19, v20: T20, v21: T21, v22: T22)(response: Future[R]) = execute(sequentialStrategy.execute22(f)(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16, v17, v18, v19, v20, v21, v22) _)(response)

    private def execute[R](f: Future[R] => Unit)(response: Future[R]){
      executor.execute(new Runnable {
        def run = f(response)
      })
    }
  }
}

trait ActorStrategySingleThreaded extends ActorStrategy{
  private lazy val futureDeliverySequential = new FutureDeliveryStrategySequential{}
  private lazy val actorExecutionSequential = new ActorExecutionStrategySingleThreaded{}

  implicit def futureDeliveryStrategy = futureDeliverySequential.futureDeliveryStrategy

  implicit def actorExecutionStrategy = actorExecutionSequential.actorExecutionStrategy
}