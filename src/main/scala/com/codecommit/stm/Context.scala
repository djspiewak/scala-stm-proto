package com.codecommit.stm

abstract class Context private[stm] () {
  private[stm] def retrieve[T](ref: Ref[T]): T
}

object Context {
  implicit object LiveContext extends Context {
    private[stm] def retrieve[T](ref: Ref[T]) = ref.value
  }
}
