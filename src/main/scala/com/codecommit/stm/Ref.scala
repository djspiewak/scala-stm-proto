package com.codecommit.stm

import java.util.concurrent.locks.{Lock, ReentrantLock}

trait Source[+T] {
  def unary_!(implicit c: Context) = get(c)
  
  def get(implicit c: Context): T
}

trait Sink[-T] {
  def :=(v: T)(implicit c: Transaction): Unit
}

final class Ref[T](v: T) extends Source[T] with Sink[T] {
  private var _contents = (v, 0)    // atomically bind value to revision
  
  private val commitLock: Lock = new ReentrantLock
  private var blocks = Set[AnyRef]()
  
  /**
   * Convenience constructor which simply utilizes the language
   * default for the given type instantiation.
   */
  def this() = this(null.asInstanceOf[T])
  
  def get(implicit c: Context) = c retrieve this
  
  def :=(v: T)(implicit c: Transaction) {
    c.store(this)(v)
  }
  
  override def toString = value.toString
  
  private[stm] def lock() {
    commitLock.lock()
  }
  
  private[stm] def unlock() {
    commitLock.unlock()
  }
  
  private[stm] def contents = _contents
  
  private[stm] def contents_=(v: (T, Int)) {
    _contents = v
    
    blocks foreach { _.notifyAll() }
  }
  
  private[stm] def value = contents._1
  private[stm] def rev = contents._2
  
  private[stm] def registerBlock(obj: AnyRef) {
    blocks.synchronized {
      blocks += obj
    }
  }
  
  private[stm] def deregisterBlock(obj: AnyRef) {
    blocks.synchronized {
      blocks -= obj
    }
  }
}

