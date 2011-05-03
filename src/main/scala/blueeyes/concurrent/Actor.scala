package blueeyes.concurrent

trait ActorStrategy{
  implicit def actorExecutionStrategy: ActorExecutionStrategy

  implicit def futureDeliveryStrategy: FutureDeliveryStrategy
}

trait Actor extends ActorStrategy{

  def constant[R](r: R): () => Future[R] = {() => Future.lift(r)}

  def constant[T1, R](r: R): (T1) => Future[R] = {(v1: T1) => Future.lift(r)}

  def lift[R] (f: () => R): () => Future[R] = () => lift(actorExecutionStrategy.execute(f) _)

  def lift1[T1, R](f: T1 => R): T1 => Future[R] = (v1: T1) => lift(actorExecutionStrategy.execute1(f)(v1) _)

  def lift2[T1, T2, R](f: (T1, T2) => R): (T1, T2) => Future[R] = (v1: T1, v2: T2) => lift(actorExecutionStrategy.execute2(f)(v1, v2) _)

  def lift3[T1, T2, T3, R](f: (T1, T2, T3) => R): (T1, T2, T3) => Future[R] = (v1: T1, v2: T2, v3: T3) => lift(actorExecutionStrategy.execute3(f)(v1, v2, v3) _)

  def lift4[T1, T2, T3, T4, R](f: (T1, T2, T3, T4) => R): (T1, T2, T3, T4) => Future[R] = (v1: T1, v2: T2, v3: T3, v4: T4) => lift(actorExecutionStrategy.execute4(f)(v1, v2, v3, v4) _)

  def lift5[T1, T2, T3, T4, T5, R](f: (T1, T2, T3, T4, T5) => R): (T1, T2, T3, T4, T5) => Future[R] = (v1: T1, v2: T2, v3: T3, v4: T4, v5: T5) => lift(actorExecutionStrategy.execute5(f)(v1, v2, v3, v4, v5) _)

  def lift6[T1, T2, T3, T4, T5, T6, R](f: (T1, T2, T3, T4, T5, T6) => R): (T1, T2, T3, T4, T5, T6) => Future[R] = (v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6) => lift(actorExecutionStrategy.execute6(f)(v1, v2, v3, v4, v5, v6) _)

  def lift7[T1, T2, T3, T4, T5, T6, T7, R](f: (T1, T2, T3, T4, T5, T6, T7) => R): (T1, T2, T3, T4, T5, T6, T7) => Future[R] = (v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7) => lift(actorExecutionStrategy.execute7(f)(v1, v2, v3, v4, v5, v6, v7) _)

  def lift8[T1, T2, T3, T4, T5, T6, T7, T8, R](f: (T1, T2, T3, T4, T5, T6, T7, T8) => R): (T1, T2, T3, T4, T5, T6, T7, T8) => Future[R] = (v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8) => lift(actorExecutionStrategy.execute8(f)(v1, v2, v3, v4, v5, v6, v7, v8) _)

  def lift9[T1, T2, T3, T4, T5, T6, T7, T8, T9, R](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9) => R): (T1, T2, T3, T4, T5, T6, T7, T8, T9) => Future[R] = (v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9) => lift(actorExecutionStrategy.execute9(f)(v1, v2, v3, v4, v5, v6, v7, v8, v9) _)

  def lift10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, R](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10) => R): (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10) => Future[R] = (v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10) => lift(actorExecutionStrategy.execute10(f)(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10) _)

  def lift11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, R](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11) => R): (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11) => Future[R] = (v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10, v11: T11) => lift(actorExecutionStrategy.execute11(f)(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11) _)

  def lift12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, R](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12) => R): (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12) => Future[R] = (v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10, v11: T11, v12: T12) => lift(actorExecutionStrategy.execute12(f)(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12) _)

  def lift13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, R](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13) => R): (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13) => Future[R] = (v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10, v11: T11, v12: T12, v13: T13) => lift(actorExecutionStrategy.execute13(f)(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13) _)

  def lift14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, R](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14) => R): (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14) => Future[R] = (v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10, v11: T11, v12: T12, v13: T13, v14: T14) => lift(actorExecutionStrategy.execute14(f)(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14) _)

  def lift15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, R](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15) => R): (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15) => Future[R] = (v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10, v11: T11, v12: T12, v13: T13, v14: T14, v15: T15) => lift(actorExecutionStrategy.execute15(f)(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15) _)

  def lift16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, R](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16) => R): (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16) => Future[R] = (v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10, v11: T11, v12: T12, v13: T13, v14: T14, v15: T15, v16: T16) => lift(actorExecutionStrategy.execute16(f)(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16) _)

  def lift17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, R](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17) => R): (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17) => Future[R] = (v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10, v11: T11, v12: T12, v13: T13, v14: T14, v15: T15, v16: T16, v17: T17) => lift(actorExecutionStrategy.execute17(f)(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16, v17) _)

  def lift18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, R](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18) => R): (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18) => Future[R] = (v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10, v11: T11, v12: T12, v13: T13, v14: T14, v15: T15, v16: T16, v17: T17, v18: T18) => lift(actorExecutionStrategy.execute18(f)(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16, v17, v18) _)

  def lift19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, R](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19) => R): (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19) => Future[R] = (v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10, v11: T11, v12: T12, v13: T13, v14: T14, v15: T15, v16: T16, v17: T17, v18: T18, v19: T19) => lift(actorExecutionStrategy.execute19(f)(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16, v17, v18, v19) _)

  def lift20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, R](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20) => R): (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20) => Future[R] = (v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10, v11: T11, v12: T12, v13: T13, v14: T14, v15: T15, v16: T16, v17: T17, v18: T18, v19: T19, v20: T20) => lift(actorExecutionStrategy.execute20(f)(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16, v17, v18, v19, v20) _)

  def lift21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, R](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21) => R): (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21) => Future[R] = (v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10, v11: T11, v12: T12, v13: T13, v14: T14, v15: T15, v16: T16, v17: T17, v18: T18, v19: T19, v20: T20, v21: T21) => lift(actorExecutionStrategy.execute21(f)(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16, v17, v18, v19, v20, v21) _)

  def lift22[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22, R](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22) => R): (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22) => Future[R] = (v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10, v11: T11, v12: T12, v13: T13, v14: T14, v15: T15, v16: T16, v17: T17, v18: T18, v19: T19, v20: T20, v21: T21, v22: T22) => lift(actorExecutionStrategy.execute22(f)(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16, v17, v18, v19, v20, v21, v22) _)

  private def lift[R](f: Future[R] => Unit): Future[R] = {
    val response = new Future[R]
    f(response)
    response
  }
}

trait ActorExecutionStrategy{
  def execute[R](f: () => R)(response: Future[R]): Unit

  def execute1[T1, R](f: T1 => R)(v1: T1)(response: Future[R]): Unit

  def execute2[T1, T2, R](f: (T1, T2) => R)(v1: T1, v2: T2)(response: Future[R]): Unit

  def execute3[T1, T2, T3, R](f: (T1, T2, T3) => R)(v1: T1, v2: T2, v3: T3)(response: Future[R]): Unit

  def execute4[T1, T2, T3, T4, R](f: (T1, T2, T3, T4) => R)(v1: T1, v2: T2, v3: T3, v4: T4)(response: Future[R]): Unit

  def execute5[T1, T2, T3, T4, T5, R](f: (T1, T2, T3, T4, T5) => R)(v1: T1, v2: T2, v3: T3, v4: T4, v5: T5)(response: Future[R]): Unit

  def execute6[T1, T2, T3, T4, T5, T6, R](f: (T1, T2, T3, T4, T5, T6) => R)(v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6)(response: Future[R]): Unit

  def execute7[T1, T2, T3, T4, T5, T6, T7, R](f: (T1, T2, T3, T4, T5, T6, T7) => R)(v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7)(response: Future[R]): Unit

  def execute8[T1, T2, T3, T4, T5, T6, T7, T8, R](f: (T1, T2, T3, T4, T5, T6, T7, T8) => R)(v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8)(response: Future[R]): Unit

  def execute9[T1, T2, T3, T4, T5, T6, T7, T8, T9, R](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9) => R)(v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9)(response: Future[R]): Unit

  def execute10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, R](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10) => R)(v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10)(response: Future[R]): Unit

  def execute11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, R](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11) => R)(v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10, v11: T11)(response: Future[R]): Unit

  def execute12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, R](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12) => R)(v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10, v11: T11, v12: T12)(response: Future[R]): Unit

  def execute13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, R](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13) => R)(v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10, v11: T11, v12: T12, v13: T13)(response: Future[R]): Unit

  def execute14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, R](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14) => R)(v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10, v11: T11, v12: T12, v13: T13, v14: T14)(response: Future[R]): Unit

  def execute15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, R](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15) => R)(v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10, v11: T11, v12: T12, v13: T13, v14: T14, v15: T15)(response: Future[R]): Unit

  def execute16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, R](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16) => R)(v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10, v11: T11, v12: T12, v13: T13, v14: T14, v15: T15, v16: T16)(response: Future[R]): Unit

  def execute17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, R](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17) => R)(v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10, v11: T11, v12: T12, v13: T13, v14: T14, v15: T15, v16: T16, v17: T17)(response: Future[R]): Unit

  def execute18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, R](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18) => R)(v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10, v11: T11, v12: T12, v13: T13, v14: T14, v15: T15, v16: T16, v17: T17, v18: T18)(response: Future[R]): Unit

  def execute19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, R](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19) => R)(v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10, v11: T11, v12: T12, v13: T13, v14: T14, v15: T15, v16: T16, v17: T17, v18: T18, v19: T19)(response: Future[R]): Unit

  def execute20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, R](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20) => R)(v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10, v11: T11, v12: T12, v13: T13, v14: T14, v15: T15, v16: T16, v17: T17, v18: T18, v19: T19, v20: T20)(response: Future[R]): Unit

  def execute21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, R](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21) => R)(v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10, v11: T11, v12: T12, v13: T13, v14: T14, v15: T15, v16: T16, v17: T17, v18: T18, v19: T19, v20: T20, v21: T21)(response: Future[R]): Unit

  def execute22[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22, R](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22) => R)(v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10, v11: T11, v12: T12, v13: T13, v14: T14, v15: T15, v16: T16, v17: T17, v18: T18, v19: T19, v20: T20, v21: T21, v22: T22)(response: Future[R]): Unit
}

trait ActorImplicit{
  implicit def richActor[T1, T2](actor: T1 => Future[T2]) = new  {
    /** Actor composition.
     *
     * {{{
     * val aToC = aToB >>> bToC
     * }}}
     */
    def >>>[T3](that: T2 => Future[T3]): T1 => Future[T3] = { v1: T1 =>
      actor(v1).flatMap[T3](that)
    }
    def flatMap[T3](that: T2 => Future[T3]): T1 => Future[T3] = { v1: T1 =>
      actor(v1).flatMap[T3](that)
    }
    def map[T3](f: T2 => T3): T1 => Future[T3] = { v1: T1 =>
      actor(v1).map[T3](f)
    }
    def flatten[T3](implicit witness: T2 => Future[T3]): T1 => Future[T3] = { v1: T1 =>
      actor(v1).flatMap[T3](witness)
    }
  }
  implicit def actorOfFutureToFlattenedActor[A, B](a: A => Future[Future[B]]): A => Future[B] = a.flatten
}

object ActorImplicit extends ActorImplicit
