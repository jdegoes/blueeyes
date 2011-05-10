package blueeyes.concurrent

trait ActorStrategy{
  implicit def actorExecutionStrategy: ActorExecutionStrategy

  implicit def futureDeliveryStrategy: FutureDeliveryStrategy
}

trait Actor extends ActorStrategy{ self =>

  def constant[R](r: R): () => Future[R] = {() => Future.lift(r)}

  def constant[T1, R](r: R): (T1) => Future[R] = {(v1: T1) => Future.lift(r)}

  def lift[R] (f: () => R): () => Future[R] = () => lift(actorExecutionStrategy.execute(self)(f) _)

  def flatLift[R](f: () => Future[R]): () => Future[R] = () => lift(f)().flatten

  def lift1[T1, R](f: T1 => R): T1 => Future[R] = (v1: T1) => lift(actorExecutionStrategy.execute1(self)(f)(v1) _)

  def flatLift1[T1, R](f: T1 => Future[R]): T1 => Future[R] = (v1: T1) => lift1(f)(v1).flatten

  def lift2[T1, T2, R](f: (T1, T2) => R): (T1, T2) => Future[R] = (v1: T1, v2: T2) => lift(actorExecutionStrategy.execute2(self)(f)(v1, v2) _)

  def flatLift2[T1, T2, R](f: (T1, T2) => Future[R]): (T1, T2) => Future[R] = (v1: T1, v2: T2) => lift2(f)(v1, v2).flatten

  def lift3[T1, T2, T3, R](f: (T1, T2, T3) => R): (T1, T2, T3) => Future[R] = (v1: T1, v2: T2, v3: T3) => lift(actorExecutionStrategy.execute3(self)(f)(v1, v2, v3) _)

  def flatLift3[T1, T2, T3, R](f: (T1, T2, T3) => Future[R]): (T1, T2, T3) => Future[R] = (v1: T1, v2: T2, v3: T3) => lift3(f)(v1, v2, v3).flatten

  def lift4[T1, T2, T3, T4, R](f: (T1, T2, T3, T4) => R): (T1, T2, T3, T4) => Future[R] = (v1: T1, v2: T2, v3: T3, v4: T4) => lift(actorExecutionStrategy.execute4(self)(f)(v1, v2, v3, v4) _)

  def flatLift4[T1, T2, T3, T4, R](f: (T1, T2, T3, T4) => Future[R]): (T1, T2, T3, T4) => Future[R] = (v1: T1, v2: T2, v3: T3, v4: T4) => lift4(f)(v1, v2, v3, v4).flatten

  def lift5[T1, T2, T3, T4, T5, R](f: (T1, T2, T3, T4, T5) => R): (T1, T2, T3, T4, T5) => Future[R] = (v1: T1, v2: T2, v3: T3, v4: T4, v5: T5) => lift(actorExecutionStrategy.execute5(self)(f)(v1, v2, v3, v4, v5) _)

  def flatLift5[T1, T2, T3, T4, T5, R](f: (T1, T2, T3, T4, T5) => Future[R]): (T1, T2, T3, T4, T5) => Future[R] = (v1: T1, v2: T2, v3: T3, v4: T4, v5: T5) => lift5(f)(v1, v2, v3, v4, v5).flatten

  def lift6[T1, T2, T3, T4, T5, T6, R](f: (T1, T2, T3, T4, T5, T6) => R): (T1, T2, T3, T4, T5, T6) => Future[R] = (v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6) => lift(actorExecutionStrategy.execute6(self)(f)(v1, v2, v3, v4, v5, v6) _)

  def flatLift6[T1, T2, T3, T4, T5, T6, R](f: (T1, T2, T3, T4, T5, T6) => Future[R]): (T1, T2, T3, T4, T5, T6) => Future[R] = (v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6) => lift6(f)(v1, v2, v3, v4, v5, v6).flatten

  def lift7[T1, T2, T3, T4, T5, T6, T7, R](f: (T1, T2, T3, T4, T5, T6, T7) => R): (T1, T2, T3, T4, T5, T6, T7) => Future[R] = (v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7) => lift(actorExecutionStrategy.execute7(self)(f)(v1, v2, v3, v4, v5, v6, v7) _)

  def flatLift7[T1, T2, T3, T4, T5, T6, T7, R](f: (T1, T2, T3, T4, T5, T6, T7) => Future[R]): (T1, T2, T3, T4, T5, T6, T7) => Future[R] = (v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7) => lift7(f)(v1, v2, v3, v4, v5, v6, v7).flatten

  def lift8[T1, T2, T3, T4, T5, T6, T7, T8, R](f: (T1, T2, T3, T4, T5, T6, T7, T8) => R): (T1, T2, T3, T4, T5, T6, T7, T8) => Future[R] = (v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8) => lift(actorExecutionStrategy.execute8(self)(f)(v1, v2, v3, v4, v5, v6, v7, v8) _)

  def flatLift8[T1, T2, T3, T4, T5, T6, T7, T8, R](f: (T1, T2, T3, T4, T5, T6, T7, T8) => Future[R]): (T1, T2, T3, T4, T5, T6, T7, T8) => Future[R] = (v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8) => lift8(f)(v1, v2, v3, v4, v5, v6, v7, v8).flatten

  def lift9[T1, T2, T3, T4, T5, T6, T7, T8, T9, R](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9) => R): (T1, T2, T3, T4, T5, T6, T7, T8, T9) => Future[R] = (v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9) => lift(actorExecutionStrategy.execute9(self)(f)(v1, v2, v3, v4, v5, v6, v7, v8, v9) _)

  def flatLift9[T1, T2, T3, T4, T5, T6, T7, T8, T9, R](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9) => Future[R]): (T1, T2, T3, T4, T5, T6, T7, T8, T9) => Future[R] = (v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9) => lift9(f)(v1, v2, v3, v4, v5, v6, v7, v8, v9).flatten

  def lift10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, R](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10) => R): (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10) => Future[R] = (v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10) => lift(actorExecutionStrategy.execute10(self)(f)(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10) _)

  def flatLift10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, R](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10) => Future[R]): (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10) => Future[R] = (v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10) => lift10(f)(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10).flatten

  def lift11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, R](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11) => R): (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11) => Future[R] = (v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10, v11: T11) => lift(actorExecutionStrategy.execute11(self)(f)(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11) _)

  def flatLift11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, R](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11) => Future[R]): (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11) => Future[R] = (v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10, v11: T11) => lift11(f)(v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10, v11: T11).flatten

  def lift12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, R](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12) => R): (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12) => Future[R] = (v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10, v11: T11, v12: T12) => lift(actorExecutionStrategy.execute12(self)(f)(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12) _)

  def flatLift12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, R](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12) => Future[R]): (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12) => Future[R] = (v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10, v11: T11, v12: T12) => lift12(f)(v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10, v11: T11, v12: T12).flatten

  def lift13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, R](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13) => R): (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13) => Future[R] = (v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10, v11: T11, v12: T12, v13: T13) => lift(actorExecutionStrategy.execute13(self)(f)(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13) _)

  def flatLift13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, R](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13) => Future[R]): (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13) => Future[R] = (v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10, v11: T11, v12: T12, v13: T13) => lift13(f)(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13).flatten

  def lift14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, R](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14) => R): (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14) => Future[R] = (v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10, v11: T11, v12: T12, v13: T13, v14: T14) => lift(actorExecutionStrategy.execute14(self)(f)(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14) _)

  def flatLift14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, R](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14) => Future[R]): (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14) => Future[R] = (v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10, v11: T11, v12: T12, v13: T13, v14: T14) => lift14(f)(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14).flatten

  def lift15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, R](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15) => R): (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15) => Future[R] = (v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10, v11: T11, v12: T12, v13: T13, v14: T14, v15: T15) => lift(actorExecutionStrategy.execute15(self)(f)(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15) _)

  def flatLift15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, R](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15) => Future[R]): (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15) => Future[R] = (v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10, v11: T11, v12: T12, v13: T13, v14: T14, v15: T15) => lift15(f)(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15).flatten

  def lift16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, R](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16) => R): (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16) => Future[R] = (v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10, v11: T11, v12: T12, v13: T13, v14: T14, v15: T15, v16: T16) => lift(actorExecutionStrategy.execute16(self)(f)(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16) _)

  def flatLift16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, R](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16) => Future[R]): (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16) => Future[R] = (v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10, v11: T11, v12: T12, v13: T13, v14: T14, v15: T15, v16: T16) => lift16(f)(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16).flatten

  def lift17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, R](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17) => R): (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17) => Future[R] = (v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10, v11: T11, v12: T12, v13: T13, v14: T14, v15: T15, v16: T16, v17: T17) => lift(actorExecutionStrategy.execute17(self)(f)(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16, v17) _)

  def flatLift17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, R](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17) => Future[R]): (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17) => Future[R] = (v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10, v11: T11, v12: T12, v13: T13, v14: T14, v15: T15, v16: T16, v17: T17) => lift17(f)(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16, v17).flatten

  def lift18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, R](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18) => R): (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18) => Future[R] = (v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10, v11: T11, v12: T12, v13: T13, v14: T14, v15: T15, v16: T16, v17: T17, v18: T18) => lift(actorExecutionStrategy.execute18(self)(f)(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16, v17, v18) _)

  def flatLift18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, R](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18) => Future[R]): (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18) => Future[R] = (v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10, v11: T11, v12: T12, v13: T13, v14: T14, v15: T15, v16: T16, v17: T17, v18: T18) => lift18(f)(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16, v17, v18).flatten

  def lift19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, R](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19) => R): (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19) => Future[R] = (v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10, v11: T11, v12: T12, v13: T13, v14: T14, v15: T15, v16: T16, v17: T17, v18: T18, v19: T19) => lift(actorExecutionStrategy.execute19(self)(f)(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16, v17, v18, v19) _)

  def flatLift19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, R](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19) => Future[R]): (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19) => Future[R] = (v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10, v11: T11, v12: T12, v13: T13, v14: T14, v15: T15, v16: T16, v17: T17, v18: T18, v19: T19) => lift19(f)(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16, v17, v18, v19).flatten

  def lift20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, R](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20) => R): (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20) => Future[R] = (v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10, v11: T11, v12: T12, v13: T13, v14: T14, v15: T15, v16: T16, v17: T17, v18: T18, v19: T19, v20: T20) => lift(actorExecutionStrategy.execute20(self)(f)(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16, v17, v18, v19, v20) _)

  def flatLift20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, R](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20) => Future[R]): (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20) => Future[R] = (v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10, v11: T11, v12: T12, v13: T13, v14: T14, v15: T15, v16: T16, v17: T17, v18: T18, v19: T19, v20: T20) => lift20(f)(v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10, v11: T11, v12: T12, v13: T13, v14: T14, v15: T15, v16: T16, v17: T17, v18: T18, v19: T19, v20: T20).flatten

  def lift21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, R](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21) => R): (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21) => Future[R] = (v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10, v11: T11, v12: T12, v13: T13, v14: T14, v15: T15, v16: T16, v17: T17, v18: T18, v19: T19, v20: T20, v21: T21) => lift(actorExecutionStrategy.execute21(self)(f)(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16, v17, v18, v19, v20, v21) _)

  def flatLift21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, R](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21) => Future[R]): (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21) => Future[R] = (v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10, v11: T11, v12: T12, v13: T13, v14: T14, v15: T15, v16: T16, v17: T17, v18: T18, v19: T19, v20: T20, v21: T21) => lift21(f)(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16, v17, v18, v19, v20, v21).flatten

  def lift22[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22, R](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22) => R): (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22) => Future[R] = (v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10, v11: T11, v12: T12, v13: T13, v14: T14, v15: T15, v16: T16, v17: T17, v18: T18, v19: T19, v20: T20, v21: T21, v22: T22) => lift(actorExecutionStrategy.execute22(self)(f)(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16, v17, v18, v19, v20, v21, v22) _)

  def flatLift22[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22, R](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22) => Future[R]): (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22) => Future[R] = (v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10, v11: T11, v12: T12, v13: T13, v14: T14, v15: T15, v16: T16, v17: T17, v18: T18, v19: T19, v20: T20, v21: T21, v22: T22) => lift22(f)(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16, v17, v18, v19, v20, v21, v22).flatten

  private def lift[R](f: Future[R] => Unit): Future[R] = {
    val response = new Future[R]
    f(response)
    response
  }
}

trait ActorExecutionStrategy{
  def execute[R](actor: Actor)(f: () => R)(response: Future[R]): Unit

  def execute1[T1, R](actor: Actor)(f: T1 => R)(v1: T1)(response: Future[R]): Unit

  def execute2[T1, T2, R](actor: Actor)(f: (T1, T2) => R)(v1: T1, v2: T2)(response: Future[R]): Unit

  def execute3[T1, T2, T3, R](actor: Actor)(f: (T1, T2, T3) => R)(v1: T1, v2: T2, v3: T3)(response: Future[R]): Unit

  def execute4[T1, T2, T3, T4, R](actor: Actor)(f: (T1, T2, T3, T4) => R)(v1: T1, v2: T2, v3: T3, v4: T4)(response: Future[R]): Unit

  def execute5[T1, T2, T3, T4, T5, R](actor: Actor)(f: (T1, T2, T3, T4, T5) => R)(v1: T1, v2: T2, v3: T3, v4: T4, v5: T5)(response: Future[R]): Unit

  def execute6[T1, T2, T3, T4, T5, T6, R](actor: Actor)(f: (T1, T2, T3, T4, T5, T6) => R)(v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6)(response: Future[R]): Unit

  def execute7[T1, T2, T3, T4, T5, T6, T7, R](actor: Actor)(f: (T1, T2, T3, T4, T5, T6, T7) => R)(v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7)(response: Future[R]): Unit

  def execute8[T1, T2, T3, T4, T5, T6, T7, T8, R](actor: Actor)(f: (T1, T2, T3, T4, T5, T6, T7, T8) => R)(v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8)(response: Future[R]): Unit

  def execute9[T1, T2, T3, T4, T5, T6, T7, T8, T9, R](actor: Actor)(f: (T1, T2, T3, T4, T5, T6, T7, T8, T9) => R)(v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9)(response: Future[R]): Unit

  def execute10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, R](actor: Actor)(f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10) => R)(v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10)(response: Future[R]): Unit

  def execute11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, R](actor: Actor)(f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11) => R)(v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10, v11: T11)(response: Future[R]): Unit

  def execute12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, R](actor: Actor)(f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12) => R)(v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10, v11: T11, v12: T12)(response: Future[R]): Unit

  def execute13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, R](actor: Actor)(f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13) => R)(v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10, v11: T11, v12: T12, v13: T13)(response: Future[R]): Unit

  def execute14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, R](actor: Actor)(f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14) => R)(v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10, v11: T11, v12: T12, v13: T13, v14: T14)(response: Future[R]): Unit

  def execute15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, R](actor: Actor)(f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15) => R)(v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10, v11: T11, v12: T12, v13: T13, v14: T14, v15: T15)(response: Future[R]): Unit

  def execute16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, R](actor: Actor)(f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16) => R)(v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10, v11: T11, v12: T12, v13: T13, v14: T14, v15: T15, v16: T16)(response: Future[R]): Unit

  def execute17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, R](actor: Actor)(f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17) => R)(v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10, v11: T11, v12: T12, v13: T13, v14: T14, v15: T15, v16: T16, v17: T17)(response: Future[R]): Unit

  def execute18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, R](actor: Actor)(f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18) => R)(v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10, v11: T11, v12: T12, v13: T13, v14: T14, v15: T15, v16: T16, v17: T17, v18: T18)(response: Future[R]): Unit

  def execute19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, R](actor: Actor)(f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19) => R)(v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10, v11: T11, v12: T12, v13: T13, v14: T14, v15: T15, v16: T16, v17: T17, v18: T18, v19: T19)(response: Future[R]): Unit

  def execute20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, R](actor: Actor)(f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20) => R)(v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10, v11: T11, v12: T12, v13: T13, v14: T14, v15: T15, v16: T16, v17: T17, v18: T18, v19: T19, v20: T20)(response: Future[R]): Unit

  def execute21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, R](actor: Actor)(f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21) => R)(v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10, v11: T11, v12: T12, v13: T13, v14: T14, v15: T15, v16: T16, v17: T17, v18: T18, v19: T19, v20: T20, v21: T21)(response: Future[R]): Unit

  def execute22[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22, R](actor: Actor)(f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22) => R)(v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10, v11: T11, v12: T12, v13: T13, v14: T14, v15: T15, v16: T16, v17: T17, v18: T18, v19: T19, v20: T20, v21: T21, v22: T22)(response: Future[R]): Unit
}

trait ActorImplicits{
  implicit def richActor[R](actor: () => Future[R]) = new  {
    def >>>[R1](that: R => Future[R1]): () => Future[R1] = () => actor().flatMap[R1](that)
    def map[R1](f: R => R1): () => Future[R1] = () => actor().map[R1](f)
  }
  implicit def richActor1[T1, R](actor: T1 => Future[R]) = new  {
    def >>>[R1](that: R => Future[R1]): T1 => Future[R1] = (v1: T1) => actor(v1).flatMap[R1](that)
    def map[R1](f: R => R1): T1 => Future[R1] = (v1: T1) => actor(v1).map[R1](f)
  }
  implicit def richActor2[T1, T2, R](actor: (T1, T2) => Future[R]) = new  {
    def >>>[R1](that: R => Future[R1]): (T1, T2) => Future[R1] = (v1: T1, v2: T2) => actor(v1, v2).flatMap[R1](that)
    def map[R1](f: R => R1): (T1, T2) => Future[R1] = (v1: T1, v2: T2) => actor(v1, v2).map[R1](f)
  }
  implicit def richActor3[T1, T2, T3, R](actor: (T1, T2, T3) => Future[R]) = new  {
    def >>>[R1](that: R => Future[R1]): (T1, T2, T3) => Future[R1] = (v1: T1, v2: T2, v3: T3) => actor(v1, v2, v3).flatMap[R1](that)
    def map[R1](f: R => R1): (T1, T2, T3) => Future[R1] = (v1: T1, v2: T2, v3: T3) => actor(v1, v2, v3).map[R1](f)
  }
  implicit def richActor4[T1, T2, T3, T4, R](actor: (T1, T2, T3, T4) => Future[R]) = new  {
    def >>>[R1](that: R => Future[R1]): (T1, T2, T3, T4) => Future[R1] = (v1: T1, v2: T2, v3: T3, v4: T4) => actor(v1, v2, v3, v4).flatMap[R1](that)
    def map[R1](f: R => R1): (T1, T2, T3, T4) => Future[R1] = (v1: T1, v2: T2, v3: T3, v4: T4) => actor(v1, v2, v3, v4).map[R1](f)
  }
  implicit def richActor5[T1, T2, T3, T4, T5, R](actor: (T1, T2, T3, T4, T5) => Future[R]) = new  {
    def >>>[R1](that: R => Future[R1]): (T1, T2, T3, T4, T5) => Future[R1] = (v1: T1, v2: T2, v3: T3, v4: T4, v5: T5) => actor(v1, v2, v3, v4, v5).flatMap[R1](that)
    def map[R1](f: R => R1): (T1, T2, T3, T4, T5) => Future[R1] = (v1: T1, v2: T2, v3: T3, v4: T4, v5: T5) => actor(v1, v2, v3, v4, v5).map[R1](f)
  }
  implicit def richActor6[T1, T2, T3, T4, T5, T6, R](actor: (T1, T2, T3, T4, T5, T6) => Future[R]) = new  {
    def >>>[R1](that: R => Future[R1]): (T1, T2, T3, T4, T5, T6) => Future[R1] = (v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6) => actor(v1, v2, v3, v4, v5, v6).flatMap[R1](that)
    def map[R1](f: R => R1): (T1, T2, T3, T4, T5, T6) => Future[R1] = (v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6) => actor(v1, v2, v3, v4, v5, v6).map[R1](f)
  }
  implicit def richActor7[T1, T2, T3, T4, T5, T6, T7, R](actor: (T1, T2, T3, T4, T5, T6, T7) => Future[R]) = new  {
    def >>>[R1](that: R => Future[R1]): (T1, T2, T3, T4, T5, T6, T7) => Future[R1] = (v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7) => actor(v1, v2, v3, v4, v5, v6, v7).flatMap[R1](that)
    def map[R1](f: R => R1): (T1, T2, T3, T4, T5, T6, T7) => Future[R1] = (v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7) => actor(v1, v2, v3, v4, v5, v6, v7).map[R1](f)
  }
  implicit def richActor8[T1, T2, T3, T4, T5, T6, T7, T8, R](actor: (T1, T2, T3, T4, T5, T6, T7, T8) => Future[R]) = new  {
    def >>>[R1](that: R => Future[R1]): (T1, T2, T3, T4, T5, T6, T7, T8) => Future[R1] = (v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8) => actor(v1, v2, v3, v4, v5, v6, v7, v8).flatMap[R1](that)
    def map[R1](f: R => R1): (T1, T2, T3, T4, T5, T6, T7, T8) => Future[R1] = (v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8) => actor(v1, v2, v3, v4, v5, v6, v7, v8).map[R1](f)
  }
  implicit def richActor9[T1, T2, T3, T4, T5, T6, T7, T8, T9, R](actor: (T1, T2, T3, T4, T5, T6, T7, T8, T9) => Future[R]) = new  {
    def >>>[R1](that: R => Future[R1]): (T1, T2, T3, T4, T5, T6, T7, T8, T9) => Future[R1] = (v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9) => actor(v1, v2, v3, v4, v5, v6, v7, v8, v9).flatMap[R1](that)
    def map[R1](f: R => R1): (T1, T2, T3, T4, T5, T6, T7, T8, T9) => Future[R1] = (v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9) => actor(v1, v2, v3, v4, v5, v6, v7, v8, v9).map[R1](f)
  }
  implicit def richActor10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, R](actor: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10) => Future[R]) = new  {
    def >>>[R1](that: R => Future[R1]): (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10) => Future[R1] = (v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10) => actor(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10).flatMap[R1](that)
    def map[R1](f: R => R1): (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10) => Future[R1] = (v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10) => actor(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10).map[R1](f)
  }
  implicit def richActor11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, R](actor: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11) => Future[R]) = new  {
    def >>>[R1](that: R => Future[R1]): (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11) => Future[R1] = (v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10, v11: T11) => actor(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11).flatMap[R1](that)
    def map[R1](f: R => R1): (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11) => Future[R1] = (v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10, v11: T11) => actor(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11).map[R1](f)
  }
  implicit def richActor12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, R](actor: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12) => Future[R]) = new  {
    def >>>[R1](that: R => Future[R1]): (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12) => Future[R1] = (v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10, v11: T11, v12: T12) => actor(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12).flatMap[R1](that)
    def map[R1](f: R => R1): (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12) => Future[R1] = (v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10, v11: T11, v12: T12) => actor(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12).map[R1](f)
  }
  implicit def richActor13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, R](actor: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13) => Future[R]) = new  {
    def >>>[R1](that: R => Future[R1]): (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13) => Future[R1] = (v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10, v11: T11, v12: T12, v13: T13) => actor(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13).flatMap[R1](that)
    def map[R1](f: R => R1): (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13) => Future[R1] = (v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10, v11: T11, v12: T12, v13: T13) => actor(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13).map[R1](f)
  }
  implicit def richActor14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, R](actor: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14) => Future[R]) = new  {
    def >>>[R1](that: R => Future[R1]): (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14) => Future[R1] = (v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10, v11: T11, v12: T12, v13: T13, v14: T14) => actor(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14).flatMap[R1](that)
    def map[R1](f: R => R1): (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14) => Future[R1] = (v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10, v11: T11, v12: T12, v13: T13, v14: T14) => actor(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14).map[R1](f)
  }
  implicit def richActor15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, R](actor: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15) => Future[R]) = new  {
    def >>>[R1](that: R => Future[R1]): (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15) => Future[R1] = (v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10, v11: T11, v12: T12, v13: T13, v14: T14, v15: T15) => actor(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15).flatMap[R1](that)
    def map[R1](f: R => R1): (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15) => Future[R1] = (v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10, v11: T11, v12: T12, v13: T13, v14: T14, v15: T15) => actor(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15).map[R1](f)
  }
  implicit def richActor16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, R](actor: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16) => Future[R]) = new  {
    def >>>[R1](that: R => Future[R1]): (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16) => Future[R1] = (v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10, v11: T11, v12: T12, v13: T13, v14: T14, v15: T15, v16: T16) => actor(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16).flatMap[R1](that)
    def map[R1](f: R => R1): (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16) => Future[R1] = (v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10, v11: T11, v12: T12, v13: T13, v14: T14, v15: T15, v16: T16) => actor(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16).map[R1](f)
  }
  implicit def richActor17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, R](actor: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17) => Future[R]) = new  {
    def >>>[R1](that: R => Future[R1]): (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17) => Future[R1] = (v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10, v11: T11, v12: T12, v13: T13, v14: T14, v15: T15, v16: T16, v17: T17) => actor(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16, v17).flatMap[R1](that)
    def map[R1](f: R => R1): (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17) => Future[R1] = (v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10, v11: T11, v12: T12, v13: T13, v14: T14, v15: T15, v16: T16, v17: T17) => actor(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16, v17).map[R1](f)
  }
  implicit def richActor18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, R](actor: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18) => Future[R]) = new  {
    def >>>[R1](that: R => Future[R1]): (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18) => Future[R1] = (v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10, v11: T11, v12: T12, v13: T13, v14: T14, v15: T15, v16: T16, v17: T17, v18: T18) => actor(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16, v17, v18).flatMap[R1](that)
    def map[R1](f: R => R1): (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18) => Future[R1] = (v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10, v11: T11, v12: T12, v13: T13, v14: T14, v15: T15, v16: T16, v17: T17, v18: T18) => actor(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16, v17, v18).map[R1](f)
  }
  implicit def richActor19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, R](actor: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19) => Future[R]) = new  {
    def >>>[R1](that: R => Future[R1]): (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19) => Future[R1] = (v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10, v11: T11, v12: T12, v13: T13, v14: T14, v15: T15, v16: T16, v17: T17, v18: T18, v19: T19) => actor(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16, v17, v18, v19).flatMap[R1](that)
    def map[R1](f: R => R1): (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19) => Future[R1] = (v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10, v11: T11, v12: T12, v13: T13, v14: T14, v15: T15, v16: T16, v17: T17, v18: T18, v19: T19) => actor(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16, v17, v18, v19).map[R1](f)
  }
  implicit def richActor20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, R](actor: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20) => Future[R]) = new  {
    def >>>[R1](that: R => Future[R1]): (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20) => Future[R1] = (v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10, v11: T11, v12: T12, v13: T13, v14: T14, v15: T15, v16: T16, v17: T17, v18: T18, v19: T19, v20: T20) => actor(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16, v17, v18, v19, v20).flatMap[R1](that)
    def map[R1](f: R => R1): (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20) => Future[R1] = (v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10, v11: T11, v12: T12, v13: T13, v14: T14, v15: T15, v16: T16, v17: T17, v18: T18, v19: T19, v20: T20) => actor(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16, v17, v18, v19, v20).map[R1](f)
  }
  implicit def richActor21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, R](actor: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21) => Future[R]) = new  {
    def >>>[R1](that: R => Future[R1]): (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21) => Future[R1] = (v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10, v11: T11, v12: T12, v13: T13, v14: T14, v15: T15, v16: T16, v17: T17, v18: T18, v19: T19, v20: T20, v21: T21) => actor(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16, v17, v18, v19, v20, v21).flatMap[R1](that)
    def map[R1](f: R => R1): (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21) => Future[R1] = (v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10, v11: T11, v12: T12, v13: T13, v14: T14, v15: T15, v16: T16, v17: T17, v18: T18, v19: T19, v20: T20, v21: T21) => actor(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16, v17, v18, v19, v20, v21).map[R1](f)
  }
  implicit def richActor22[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22, R](actor: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22) => Future[R]) = new  {
    def >>>[R1](that: R => Future[R1]): (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22) => Future[R1] = (v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10, v11: T11, v12: T12, v13: T13, v14: T14, v15: T15, v16: T16, v17: T17, v18: T18, v19: T19, v20: T20, v21: T21, v22: T22) => actor(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16, v17, v18, v19, v20, v21, v22).flatMap[R1](that)
    def map[R1](f: R => R1): (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22) => Future[R1] = (v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10, v11: T11, v12: T12, v13: T13, v14: T14, v15: T15, v16: T16, v17: T17, v18: T18, v19: T19, v20: T20, v21: T21, v22: T22) => actor(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16, v17, v18, v19, v20, v21, v22).map[R1](f)
  }
}

object ActorImplicits extends ActorImplicits
