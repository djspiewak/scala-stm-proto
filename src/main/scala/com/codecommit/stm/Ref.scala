package com.codecommit.stm

class Ref[T](v: T) {
  private var contents_ = (v, 0)    // atomically bind value to revision
  
  private var blocks = Set[AnyRef]()
  
  /**
   * Convenience constructor which simply utilizes the language
   * default for the given type instantiation.
   */
  def this() = this(null.asInstanceOf[T])
  
  /**
   * Alright, ballot time: who wants a unary_*(Context) method
   * as syntax sugar for this?  Anyone?  :-)
   */
  def get(implicit c: Context) = c retrieve this
  
  def :=(v: T)(implicit c: Transaction) {
    c.store(this)(v)
  }
  
  override def toString = value.toString
  
  private[stm] def contents = contents_
  
  private[stm] def contents_=(v: (T, Int)) {
    contents_ = v
    
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

